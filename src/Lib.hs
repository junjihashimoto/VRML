{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where


import Data.Int
import Data.Void
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text hiding (empty)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

type Parser = Parsec Void String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexm :: Parser a -> Parser a
lexm = L.lexeme sc

space' = void $ takeWhileP (Just "white space") (\c -> c == '\t' || c == ' ')

sc' :: Parser ()
sc' = L.space space' empty empty

lexm' :: Parser a -> Parser a
lexm' = L.lexeme sc'

data VrmlScene = VrmlScene [Statement]
  deriving (Show,Eq)

-- | parser of VrmlScene
--
-- >>> parseTest parseVrmlScene ""
-- VrmlScene []
parseVrmlScene :: Parser VrmlScene
parseVrmlScene = VrmlScene <$> many parseStatement

data Statement
  = StatementNodeStatement NodeStatement
  | StatementProtoStatement ProtoStatement
  | StatementRoute Route
  deriving (Show,Eq)

parseStatement :: Parser Statement
parseStatement = 
  (StatementRoute <$> parseRoute) <|>
  (StatementNodeStatement  <$> parseNodeStatement) <|>
  (StatementProtoStatement <$> parseProtoStatement)

data NodeStatement
  = NodeStatement Node
  | DEF NodeNameId Node
  | USE NodeNameId
  deriving (Show,Eq)

-- | parser of Node
--
-- >>> parseTest parseNodeStatement "hoge {}"
-- NodeStatement (Node (NodeTypeId "hoge") [])
-- >>> parseTest parseNodeStatement "DEF hoge1 hoge {}"
-- DEF (NodeNameId "hoge1") (Node (NodeTypeId "hoge") [])
-- >>> parseTest parseNodeStatement "USE hoge1"
-- USE (NodeNameId "hoge1")
parseNodeStatement :: Parser NodeStatement
parseNodeStatement = 
  (DEF <$> (lstring "DEF" >> parseNodeNameId) <*> parseNode) <|>
  (USE <$> (lstring "USE" >> parseNodeNameId)) <|>
  (NodeStatement <$> parseNode)

data ProtoStatement
  = Proto NodeTypeId [InterfaceDeclaration] ProtoBody
  | ExternProto NodeTypeId [ExternInterfaceDeclaration] URLList
  deriving (Show,Eq)

-- | parser of Proto
--
-- >>> parseTest parseProtoStatement "PROTO Cube [] { Box {} }"
-- Proto (NodeTypeId "Cube") [] (ProtoBody [] (Node (NodeTypeId "Box") []) [])
parseProtoStatement :: Parser ProtoStatement
parseProtoStatement = 
  (id (Proto
   <$> (lstring "PROTO" >> parseNodeTypeId)
   <*> (lstring "[" >> many parseInterfaceDeclaration >>= \v -> lstring "]" >> pure v )
   <*> (lstring "{" >> parseProtoBody >>= \v -> lstring "}" >> pure v))) <|>
  (id (ExternProto
   <$> (lstring "EXTERNPROTO" >> parseNodeTypeId)
   <*> (lstring "[" >> many parseExternInterfaceDeclaration >>= \v -> lstring "]" >> pure v )
   <*> parseURLList))

data ProtoBody
  = ProtoBody [ProtoStatement] Node [Statement]
  deriving (Show,Eq)

-- | parser of ProtoBody
--
-- >>> parseTest parseProtoBody "hoge {}"
-- ProtoBody [] (Node (NodeTypeId "hoge") []) []
parseProtoBody :: Parser ProtoBody
parseProtoBody = do
  ProtoBody <$> many parseProtoStatement <*> parseNode <*> many parseStatement


data RestrictedInterfaceDeclaration
  = RestrictedInterfaceDeclarationEventIn FieldType EventInId
  | RestrictedInterfaceDeclarationEventOut FieldType EventOutId
  | RestrictedInterfaceDeclarationField FieldType FieldId FieldValue
  deriving (Show,Eq)

parseRestrictedInterfaceDeclaration :: Parser RestrictedInterfaceDeclaration
parseRestrictedInterfaceDeclaration =
  ( RestrictedInterfaceDeclarationEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( RestrictedInterfaceDeclarationEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( RestrictedInterfaceDeclarationField <$> (lstring "field" >> parseFieldType) <*> parseFieldId <*> parseFieldValue)
data InterfaceDeclaration
  = InterfaceDeclaration RestrictedInterfaceDeclaration
  | InterfaceDeclarationExposedField FieldType FieldId FieldValue
  deriving (Show,Eq)

parseInterfaceDeclaration :: Parser InterfaceDeclaration
parseInterfaceDeclaration =
  ( InterfaceDeclarationExposedField <$> (lstring "exposedField" >> parseFieldType) <*> parseFieldId <*> parseFieldValue) <|>
  ( InterfaceDeclaration <$> parseRestrictedInterfaceDeclaration)

data ExternInterfaceDeclaration
  = ExternInterfaceDeclarationEventIn FieldType EventInId
  | ExternInterfaceDeclarationEventOut FieldType EventOutId
  | ExternInterfaceDeclarationField FieldType FieldId
  | ExternInterfaceDeclarationExposedField FieldType FieldId
  deriving (Show,Eq)

parseExternInterfaceDeclaration :: Parser ExternInterfaceDeclaration
parseExternInterfaceDeclaration =
  ( ExternInterfaceDeclarationEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( ExternInterfaceDeclarationEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( ExternInterfaceDeclarationField <$> (lstring "field" >> parseFieldType) <*> parseFieldId) <|>
  ( ExternInterfaceDeclarationExposedField <$> (lstring "exposedField" >> parseFieldType) <*> parseFieldId)

data Route
  = Route NodeNameId EventOutId NodeNameId EventInId
  deriving (Show,Eq)

-- | parser of Route
--
-- >>> parseTest parseRoute "ROUTE hoge.hoge TO hoge.hoge"
-- Route (NodeNameId "hoge") (EventOutId "hoge") (NodeNameId "hoge") (EventInId "hoge")
parseRoute :: Parser Route
parseRoute = 
  Route
  <$> (lstring "ROUTE" >> parseNodeNameId)
  <*> (lstring "." >> parseEventOutId)
  <*> (lstring "TO" >> parseNodeNameId)
  <*> (lstring "." >> parseEventInId)

data URLList = URLList [String]
  deriving (Show,Eq)

parseURLList :: Parser URLList
parseURLList =  
  (stringLiteral >>= \v -> pure (URLList [v])) <|>
  (lstring "[" >> many stringLiteral >>= \v -> lstring "]" >> pure (URLList v))

data Node
  = Node NodeTypeId [NodeBodyElement]
  | Script [ScriptBodyElement]
  deriving (Show,Eq)

-- | parser of Node
--
-- >>> parseTest parseNode "hoge {hoge 1 hoge 2}"
-- Node (NodeTypeId "hoge") [NodeBodyElementFV (FieldId "hoge") (SffloatValue 1.0),NodeBodyElementFV (FieldId "hoge") (SffloatValue 2.0)]
-- >>> parseTest parseNode "BmwX5 { translation -78.7 0.4 7.53 }"
-- Node (NodeTypeId "BmwX5") [NodeBodyElementFV (FieldId "translation") (Sfvec3fValue (-78.7,0.4,7.53))]
-- >>> parseTest parseNode "BmwX5 { rotation 0 1 0 1.5708}"
-- Node (NodeTypeId "BmwX5") [NodeBodyElementFV (FieldId "rotation") (SfrotationValue (0.0,1.0,0.0,1.5708))]
-- >>> parseTest parseNode "BmwX5 { controller \"autonomous_vehicle\" }"
-- Node (NodeTypeId "BmwX5") [NodeBodyElementFV (FieldId "controller") (SfstringValue "autonomous_vehicle")]
-- >>> parseTest parseNode "BmwX5 { translation -78.7 0.4 7.53  rotation 0 1 0 1.5708 controller \"autonomous_vehicle\" }"
-- Node (NodeTypeId "BmwX5") [NodeBodyElementFV (FieldId "translation") (Sfvec3fValue (-78.7,0.4,7.53)),NodeBodyElementFV (FieldId "rotation") (SfrotationValue (0.0,1.0,0.0,1.5708)),NodeBodyElementFV (FieldId "controller") (SfstringValue "autonomous_vehicle")]
-- >>> parseTest parseNode "Script {}"
-- Script []
-- >>> parseTest parseNode "Script {  }"
-- Script []
parseNode :: Parser Node
parseNode = do
  nid <- parseNodeTypeId
  case nid of
    (NodeTypeId "Script") -> do
      _ <- lstring "{"
      nbody <- many parseScriptBodyElement
      _ <- lstring "}"
      return $ Script nbody
    _ -> do
      _ <- lstring "{"
      nbody <- many parseNodeBodyElement
      _ <- lstring "}"
      return $ Node nid nbody

instance Pretty Node where
  pretty (Node (NodeTypeId nid) nbody) =
    pretty nid <> "{" <> line
--    <+> pretty nbody
    <> "}" <> line

data ScriptBodyElement
  = ScriptBodyElementN NodeBodyElement
  | ScriptBodyElementR RestrictedInterfaceDeclaration
  | ScriptBodyElementEI FieldType EventInId EventInId
  | ScriptBodyElementEO FieldType EventOutId EventOutId
  | ScriptBodyElementF FieldType FieldId FieldId
  deriving (Show,Eq)

parseScriptBodyElement :: Parser ScriptBodyElement  
parseScriptBodyElement = 
  (ScriptBodyElementEI <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId <*> (lstring "IS" >> parseEventInId) ) <|>
  (ScriptBodyElementEO <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId <*> (lstring "IS" >> parseEventOutId) ) <|>
  (ScriptBodyElementF <$> (lstring "field" >> parseFieldType) <*> parseFieldId <*> (lstring "IS" >> parseFieldId)) <|>
  (ScriptBodyElementR <$> parseRestrictedInterfaceDeclaration) <|>
  (ScriptBodyElementN <$> parseNodeBodyElement)

data NodeBodyElement
  = NodeBodyElementFV FieldId FieldValue
  | NodeBodyElementFI FieldId FieldId
  | NodeBodyElementEI EventInId EventInId
  | NodeBodyElementEO EventOutId EventOutId
  | NodeBodyElementR Route
  | NodeBodyElementP ProtoStatement
  deriving (Show,Eq)

-- >>> parseTest parseBodyElement "maxPosition 1e4 1e4"
-- NodeBodyElementFV (FieldId "maxPosition") (Sfvec2fValue (1e4,1e4))
-- >>> parseTest parseBodyElement "width IS width"
-- NodeBodyElementFI (FieldId "width") (FieldId "width")
parseNodeBodyElement :: Parser NodeBodyElement  
parseNodeBodyElement = 
  (NodeBodyElementR <$> parseRoute) <|>
  (NodeBodyElementP <$> parseProtoStatement) <|>
  (try $ (NodeBodyElementFI <$> parseFieldId <*> (lstring "IS" >> parseFieldId) )) <|>
  (try $ (NodeBodyElementEI <$> parseEventInId <*> (lstring "IS" >> parseEventInId) )) <|>
  (try $ (NodeBodyElementEO <$> parseEventOutId <*> (lstring "IS" >> parseEventOutId) )) <|>
  (NodeBodyElementFV <$> parseFieldId <*> parseFieldValue)

data NodeNameId = NodeNameId String
  deriving (Show,Eq)

data NodeTypeId = NodeTypeId String
  deriving (Show,Eq)

data FieldId = FieldId String
  deriving (Show,Eq)

data EventInId = EventInId String
  deriving (Show,Eq)

data EventOutId = EventOutId String
  deriving (Show,Eq)

parseNodeNameId :: Parser NodeNameId
parseNodeNameId = NodeNameId <$> identifier

parseNodeTypeId :: Parser NodeTypeId
parseNodeTypeId = NodeTypeId <$> identifier

parseFieldId :: Parser FieldId
parseFieldId = FieldId <$> identifier

parseEventInId :: Parser EventInId
parseEventInId = EventInId <$> identifier

parseEventOutId :: Parser EventOutId
parseEventOutId = EventOutId <$> identifier


rws :: [String]
rws = []

-- | parser of Id
--
-- >>> parseTest identifier "hogehoge"
-- "hogehoge"
identifier :: Parser String
identifier = (lexm . try) (p >>= check)
 where
  p = (:) <$> (oneOf identStart) <*> many (oneOf identLetter)
  check x = if x `elem` rws
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

identStart :: [Char]
identStart = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

identLetter :: [Char]
identLetter = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9'] ++ [':', '<', '>']

lstring = lexm.string

data FieldType
  = MFColor
  | MFFloat
  | MFString
  | MFTime
  | MFVec2f
  | MFVec3f
  | SFBool
  | SFColor
  | SFFloat
  | SFImage
  | SFInt32
  | SFNode
  | SFRotation
  | SFString
  | SFTime
  | SFVec2f
  | SFVec3f
  deriving (Show,Eq)

-- | parser of FieldType
--
-- >>> parseTest parseFieldType "MFColor"
-- MFColor
-- >>> parseTest parseFieldType "MFString"
-- MFString
-- >>> parseTest parseFieldType "SFColor"
-- SFColor
-- >>> parseTest parseFieldType "MFColor "
-- MFColor
parseFieldType :: Parser FieldType
parseFieldType
  = (lstring "MFColor" >> pure MFColor)
  <|> (lstring "MFFloat" >> pure MFFloat)
  <|> (lstring "MFString" >> pure MFString)
  <|> (lstring "MFTime" >> pure MFTime)
  <|> (lstring "MFVec2f" >> pure MFVec2f)
  <|> (lstring "MFVec3f" >> pure MFVec3f)
  <|> (lstring "SFBool" >> pure SFBool)
  <|> (lstring "SFColor" >> pure SFColor)
  <|> (lstring "SFFloat" >> pure SFFloat)
  <|> (lstring "SFImage" >> pure SFImage)
  <|> (lstring "SFInt32" >> pure SFInt32)
  <|> (lstring "SFNode" >> pure SFNode)
  <|> (lstring "SFRotation" >> pure SFRotation)
  <|> (lstring "SFString" >> pure SFString)
  <|> (lstring "SFTime" >> pure SFTime)
  <|> (lstring "SFVec2f" >> pure SFVec2f)
  <|> (lstring "SFVec3f" >> pure SFVec3f)

data FieldValue
  = SfboolValue Bool
  | SfcolorValue (Float,Float,Float)
  | SffloatValue Float
  | SfimageValue [Int32]
  | Sfint32Value Int32
  | SfnodeValue (Maybe NodeStatement)
  | SfrotationValue (Float,Float,Float,Float)
  | SfstringValue String
  | SftimeValue Double
  | Sfvec2fValue (Float,Float)
  | Sfvec3fValue (Float,Float,Float)
  | MfcolorValue [(Float,Float,Float)]
  | MffloatValue [Float]
  | Mfint32Value [Int32]
  | MfnodeValue [NodeStatement]
  | MfrotationValue [(Float,Float,Float,Float)]
  | MfstringValue [String]
  | MftimeValue [Double]
  | Mfvec2fValue [(Float,Float)]
  | Mfvec3fValue [(Float,Float,Float)]
  deriving (Show,Eq)

parseFloat :: Parser Float
parseFloat =realToFrac <$> lexm pfloat

parseFloat' :: Parser Float
parseFloat' =realToFrac <$> lexm' pfloat

parseInt :: Parser Int32
parseInt = fromIntegral <$> lexm pinteger

-- | parser of FieldType
--
-- >>> parseTest tupleParser "1e4 1e4"
-- (10000.0,10000.0)
tupleParser :: Parser (Float,Float)
tupleParser = (,) <$> parseFloat <*> parseFloat

-- | parser of FieldType
--
-- >>> parseTest parseFieldValue "TRUE"
-- SfboolValue True
-- >>> parseTest parseFieldValue "FALSE"
-- SfboolValue False
-- >>> parseTest parseFieldValue "NULL"
-- SfnodeValue Nothing
-- >>> parseTest parseFieldValue "\"hoge\\\"hoge\""
-- SfstringValue "hoge\"hoge"
-- >>> parseTest parseFieldValue "\"autonomous_vehicle\""
-- SfstringValue "autonomous_vehicle"
-- >>> parseTest parseFieldValue "1e4 1e4"
-- Sfvec2fValue (10000.0,10000.0)
-- >>> parseTest parseFieldValue "[1e4 1e4 1e4,1e4 1e4 1e4]"
-- Mfvec3fValue [(10000.0,10000.0,10000.0),(10000.0,10000.0,10000.0)]
-- >>> parseTest ((,) <$> parseFloat' <*> (eol >> parseFloat)) "1e4\n1e4"
-- (10000.0,10000.0)
parseFieldValue :: Parser FieldValue
parseFieldValue
  =   (lstring "TRUE" >> pure (SfboolValue True))
  <|> (lstring "FALSE" >> pure (SfboolValue False))
  <|> (lstring "NULL" >> pure (SfnodeValue Nothing))
  <|> (try $ MfrotationValue <$> parseArray ((,,,) <$> parseFloat <*> parseFloat <*> parseFloat <*> parseFloat))
--  <|> (try $ Mfvec3fValue <$> parseArrayN ((,,) <$> parseFloat' <*> parseFloat' <*> parseFloat'))
  <|> (try $ Mfvec3fValue <$> parseArray ((,,) <$> parseFloat <*> parseFloat <*> parseFloat))
  <|> (try $ Mfvec2fValue <$> parseArray ((,) <$> parseFloat <*> parseFloat))
  <|> (try $ MffloatValue <$> parseArray parseFloat)
  <|> (try $ MfnodeValue <$> parseArray' parseNodeStatement)
  <|> (try $ (\a b c d -> SfrotationValue (a,b,c,d)) <$> parseFloat <*> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b c -> Sfvec3fValue (a,b,c)) <$> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b -> Sfvec2fValue (a,b)) <$> parseFloat <*> parseFloat)
  <|> (try $ (SffloatValue <$> parseFloat))
  <|> (try $ (stringLiteral >>= \v -> pure (SfstringValue v)))
  <|> (try $ ((SfnodeValue . Just) <$> parseNodeStatement))

parseArray :: Parser a -> Parser [a]
parseArray parser = do
  _ <- lstring "["
  values <-  parser `sepBy` lstring ","
  _ <- lstring "]"
  return values

parseArrayN :: Parser a -> Parser [a]
parseArrayN parser = do
  _ <- lstring "["
  values <-  parser `sepBy` eol
  _ <- lstring "]"
  return values

parseArray' :: Parser a -> Parser [a]
parseArray' parser = do
  _ <- lstring "["
  values <-  many parser
  _ <- lstring "]"
  return values

pinteger :: Parser Integer
pinteger =
  (try L.hexadecimal) <|>
  (L.decimal) <|>
  ((string "-") >> (try L.hexadecimal <|> L.decimal) >>= \v -> pure (-v))

pfloat :: Parser Float
pfloat =
  (realToFrac <$> L.scientific) <|>
  ((string "-") >> L.scientific >>= \v -> realToFrac <$> (pure (-v)))

-- | parser of FieldType
--
-- >>> parseTest stringLiteral "\"hoge\\\"hoge\""
-- "hoge\"hoge"
-- >>> parseTest stringLiteral "\"autonomous_vehicle\""
-- "autonomous_vehicle"
stringLiteral :: Parser String
stringLiteral = lexm $ char '\"' *> manyTill charLiteral (char '\"')
