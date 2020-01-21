{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where


import Data.Int
import Data.Void
import Data.Text hiding (empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

type Parser = Parsec Void String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sc :: Parser ()
sc = L.space space1 empty empty

lexm :: Parser a -> Parser a
lexm = L.lexeme sc

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
  | StatementRouteStatement RouteStatement
  deriving (Show,Eq)

parseStatement :: Parser Statement
parseStatement = 
  (StatementNodeStatement  <$> parseNodeStatement) <|>
  (StatementProtoStatement <$> parseProtoStatement) <|>
  (StatementRouteStatement <$> parseRouteStatement)

data NodeStatement
  = NodeStatementN Node
  | NodeStatementD NodeNameId Node
  | NodeStatementU NodeNameId
  deriving (Show,Eq)

-- | parser of Node
--
-- >>> parseTest parseNodeStatement "hoge {}"
-- NodeStatementN (Node (NodeTypeId "hoge") [])
-- >>> parseTest parseNodeStatement "DEF hoge1 hoge {}"
-- NodeStatementD (NodeNameId "hoge1") (Node (NodeTypeId "hoge") [])
-- >>> parseTest parseNodeStatement "USE hoge1"
-- NodeStatementU (NodeNameId "hoge1")
parseNodeStatement :: Parser NodeStatement
parseNodeStatement = 
  (NodeStatementD <$> (lstring "DEF" >> parseNodeNameId) <*> parseNode) <|>
  (NodeStatementU <$> (lstring "USE" >> parseNodeNameId)) <|>
  (NodeStatementN <$> parseNode)

data ProtoStatement
  = Proto NodeTypeId [InterfaceDeclaration] ProtoBody
  | ExternProto NodeTypeId [ExternInterfaceDeclaration] URLList
  deriving (Show,Eq)

-- | parser of Node
--
-- >>> parseTest parseProtoStatement "PROTO hoge [] {}"
-- Proto (NodeTypeId "hoge") [])
parseProtoStatement :: Parser ProtoStatement
parseProtoStatement = 
  (Proto
   <$> (lstring "PROTO" >> parseNodeTypeId)
   <*> (lstring "[" >> many parseInterfaceDeclaration >>= \v -> lstring "]" >> pure v )
   <*> (lstring "{" >> parseProtoBody >>= \v -> lstring "}" >> pure v)) <|>
  (ExternProto
   <$> (string "EXTERNPROTO" >> parseNodeTypeId)
   <*> (string "[" >> many parseExternInterfaceDeclaration >>= \v -> string "]" >> pure v )
   <*> parseURLList)

data ProtoBody
  = ProtoBody [ProtoStatement] Node [Statement]
  deriving (Show,Eq)

-- | parser of ProtoBody
--
-- >>> parseTest parseProtoBody "hoge {}"
-- ProtoBody [] (Node (NodeTypeId "hoge") []) []
parseProtoBody :: Parser ProtoBody
parseProtoBody =
  ProtoBody <$> many parseProtoStatement <*> parseNode <*> many parseStatement


data RestrictedInterfaceDeclaration
  = RestrictedInterfaceDeclarationEventIn FieldType EventInId
  | RestrictedInterfaceDeclarationEventOut FieldType EventOutId
  | RestrictedInterfaceDeclarationField FieldType FieldId FieldValue
  deriving (Show,Eq)

parseRestrictedInterfaceDeclaration :: Parser RestrictedInterfaceDeclaration
parseRestrictedInterfaceDeclaration =
  ( RestrictedInterfaceDeclarationEventIn <$> (string "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( RestrictedInterfaceDeclarationEventOut <$> (string "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( RestrictedInterfaceDeclarationField <$> (string "field" >> parseFieldType) <*> parseFieldId <*> parseFieldValue)
data InterfaceDeclaration
  = InterfaceDeclaration RestrictedInterfaceDeclaration
  | InterfaceDeclarationExposedField FieldType FieldId FieldValue
  deriving (Show,Eq)

parseInterfaceDeclaration :: Parser InterfaceDeclaration
parseInterfaceDeclaration =
  ( InterfaceDeclaration <$> parseRestrictedInterfaceDeclaration) <|>
  ( InterfaceDeclarationExposedField <$> (string "exposedField" >> parseFieldType) <*> parseFieldId <*> parseFieldValue)

data ExternInterfaceDeclaration
  = ExternInterfaceDeclarationEventIn FieldType EventInId
  | ExternInterfaceDeclarationEventOut FieldType EventOutId
  | ExternInterfaceDeclarationField FieldType FieldId
  | ExternInterfaceDeclarationExposedField FieldType FieldId
  deriving (Show,Eq)

parseExternInterfaceDeclaration :: Parser ExternInterfaceDeclaration
parseExternInterfaceDeclaration =
  ( ExternInterfaceDeclarationEventIn <$> (string "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( ExternInterfaceDeclarationEventOut <$> (string "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( ExternInterfaceDeclarationField <$> (string "field" >> parseFieldType) <*> parseFieldId) <|>
  ( ExternInterfaceDeclarationExposedField <$> (string "exposedField" >> parseFieldType) <*> parseFieldId)

data RouteStatement
  = RouteStatement NodeNameId EventOutId NodeNameId EventInId
  deriving (Show,Eq)

parseRouteStatement :: Parser RouteStatement
parseRouteStatement = 
  RouteStatement
  <$> (string "ROUTE" >> parseNodeNameId)
  <*> (string "." >> parseEventOutId)
  <*> (string "TO" >> parseNodeNameId)
  <*> (string "." >> parseEventInId)

data URLList = URLList [String]
  deriving (Show,Eq)

parseURLList :: Parser URLList
parseURLList =  
  (stringLiteral >>= \v -> pure (URLList [v])) <|>
  (string "[" >> many stringLiteral >>= \v -> string "]" >> pure (URLList v))

data Node
  = Node NodeTypeId [NodeBodyElement]
  | Script [ScriptBodyElement]
  deriving (Show,Eq)

-- | parser of Node
--
-- >>> parseTest parseNode "hoge {}"
-- Node (NodeTypeId "hoge") []
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
  (ScriptBodyElementN <$> parseNodeBodyElement) <|>
  (ScriptBodyElementR <$> parseRestrictedInterfaceDeclaration) <|>
  (ScriptBodyElementEI <$> (string "eventIn" >> parseFieldType) <*> parseEventInId <*> (string "IS" >> parseEventInId) ) <|>
  (ScriptBodyElementEO <$> (string "eventOut" >> parseFieldType) <*> parseEventOutId <*> (string "IS" >> parseEventOutId) ) <|>
  (ScriptBodyElementF <$> (string "field" >> parseFieldType) <*> parseFieldId <*> (string "IS" >> parseFieldId))

data NodeBodyElement
  = NodeBodyElementFV FieldId FieldValue
  | NodeBodyElementFI FieldId FieldId
  | NodeBodyElementEI EventInId EventInId
  | NodeBodyElementEO EventOutId EventOutId
  | NodeBodyElementR RouteStatement
  | NodeBodyElementP ProtoStatement
  deriving (Show,Eq)

-- >>> parseTest parseBodyElement "maxPosition 1e4 1e4"
-- NodeBodyElementFV (FieldId "maxPosition") (Sfvec2fValue (1e4,1e4))
parseNodeBodyElement :: Parser NodeBodyElement  
parseNodeBodyElement = 
  (NodeBodyElementFV <$> parseFieldId <*> parseFieldValue) <|>
  (NodeBodyElementFI <$> parseFieldId <*> (string "IS" >> parseFieldId) ) <|>
  (NodeBodyElementEI <$> parseEventInId <*> (string "IS" >> parseEventInId) ) <|>
  (NodeBodyElementEO <$> parseEventOutId <*> (string "IS" >> parseEventOutId) ) <|>
  (NodeBodyElementR <$> parseRouteStatement) <|>
  (NodeBodyElementP <$> parseProtoStatement)

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
parseFloat = lexm L.scientific >>= pure.realToFrac

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
-- >>> parseTest parseFieldValue "1e4 1e4"
-- Sfvec2fValue (10000.0,10000.0)
-- >>> parseTest parseFieldValue "[1e4 1e4 1e4,1e4 1e4 1e4]"
-- Mfvec3fValue [(10000.0,10000.0,10000.0),(10000.0,10000.0,10000.0)]
parseFieldValue :: Parser FieldValue
parseFieldValue
  =   (string "TRUE" >> pure (SfboolValue True))
  <|> (string "FALSE" >> pure (SfboolValue False))
  <|> (string "NULL" >> pure (SfnodeValue Nothing))
  <|> (try $ (\a b c d -> SfrotationValue (a,b,c,d)) <$> parseFloat <*> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b c -> Sfvec3fValue (a,b,c)) <$> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b -> Sfvec2fValue (a,b)) <$> parseFloat <*> parseFloat)
  <|> (try $ (SffloatValue <$> parseFloat))
  <|> (try $ (stringLiteral >>= \v -> pure (SfstringValue v)))
  <|> (try $ do
          _ <- lstring "["
          values <- sepBy
                    ((\a b c -> (a,b,c)) <$> parseFloat <*> parseFloat <*> parseFloat)
                    (lstring ",")
          _ <- lstring "]"
          return $ Mfvec3fValue values
      )
  <|> (try $ do
          _ <- lstring "["
          values <- sepBy
                    ((\a b -> (a,b)) <$> parseFloat <*> parseFloat)
                    (lstring ",")
          _ <- lstring "]"
          return $ Mfvec2fValue values
      )
  <|> (try $ do
          _ <- lstring "["
          values <- sepBy parseFloat (lstring ",")
          _ <- lstring "]"
          return $ MffloatValue values
      )

pinteger :: Parser Integer
pinteger =
  (try L.hexadecimal) <|>
  (L.decimal) <|>
  ((string "-") >> (try L.hexadecimal <|> L.decimal) >>= \v -> pure (-v))

-- | parser of FieldType
--
-- >>> parseTest stringLiteral "\"hoge\\\"hoge\""
-- "hoge\"hoge"
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill charLiteral (char '\"')
