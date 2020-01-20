{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import Data.Int
import Data.Void
import Data.Text hiding (empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
  
type Parser = Parsec Void String

someFunc :: IO ()
someFunc = putStrLn "someFunc"


sc :: Parser ()
sc = L.space space1 empty empty

lexm :: Parser a -> Parser a
lexm = L.lexeme sc

data VrmlScene = VrmlScene [Statement]
  deriving (Show,Eq)

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

parseNodeStatement :: Parser NodeStatement
parseNodeStatement = 
  (NodeStatementN <$> parseNode) <|>
  (NodeStatementD <$> (string "DEF" >> parseNodeNameId) <*> parseNode) <|>
  (NodeStatementU <$> (string "USE" >> parseNodeNameId))

data ProtoStatement
  = Proto NodeTypeId [InterfaceDeclaration] ProtoBody
  | ExternProto NodeTypeId [ExternInterfaceDeclaration] URLList
  deriving (Show,Eq)

parseProtoStatement :: Parser ProtoStatement
parseProtoStatement = 
  (Proto
   <$> (string "PROTO" >> parseNodeTypeId)
   <*> (string "[" >> many parseInterfaceDeclaration >>= \v -> string "]" >> pure v )
   <*> (string "{" >> parseProtoBody >>= \v -> string "}" >> pure v)) <|>
  (ExternProto
   <$> (string "EXTERNPROTO" >> parseNodeTypeId)
   <*> (string "[" >> many parseExternInterfaceDeclaration >>= \v -> string "]" >> pure v )
   <*> parseURLList)

data ProtoBody
  = ProtoBody [ProtoStatement] Node [Statement]
  deriving (Show,Eq)

parseProtoBody :: Parser ProtoBody
parseProtoBody =
  ProtoBody <$> (many parseProtoStatement) <*> parseNode <*> many parseStatement


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

parseNode :: Parser Node
parseNode = do
  nid <- parseNodeTypeId
  _ <- string "{"
  nbody <- many parseNodeBodyElement
  _ <- string "}"
  return $ Node nid nbody


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
-- >>> parseTest parseFieldType " MFColor "
-- MFColor
parseFieldType :: Parser FieldType
parseFieldType
  = (string "MFColor" >> pure MFColor)
  <|> (string "MFFloat" >> pure MFFloat)
  <|> (string "MFString" >> pure MFString)
  <|> (string "MFTime" >> pure MFTime)
  <|> (string "MFVec2f" >> pure MFVec2f)
  <|> (string "MFVec3f" >> pure MFVec3f)
  <|> (string "SFBool" >> pure SFBool)
  <|> (string "SFColor" >> pure SFColor)
  <|> (string "SFFloat" >> pure SFFloat)
  <|> (string "SFImage" >> pure SFImage)
  <|> (string "SFInt32" >> pure SFInt32)
  <|> (string "SFNode" >> pure SFNode)
  <|> (string "SFRotation" >> pure SFRotation)
  <|> (string "SFString" >> pure SFString)
  <|> (string "SFTime" >> pure SFTime)
  <|> (string "SFVec2f" >> pure SFVec2f)
  <|> (string "SFVec3f" >> pure SFVec3f)

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
-- >>> parseTest parseFieldValue "1.0"
-- SffloatValue 1.0
-- >>> parseTest parseFieldValue "0x1"
-- SffloatValue 1.0

parseFieldValue :: Parser FieldValue
parseFieldValue
  =   (string "TRUE" >> pure (SfboolValue True))
  <|> (string "FALSE" >> pure (SfboolValue False))
  <|> (string "NULL" >> pure (SfnodeValue Nothing))
  <|> (stringLiteral >>= \v -> pure (SfstringValue v))
  <|> (try (pinteger >>= \v -> pure (Sfint32Value (fromIntegral v))))
  <|> (try (L.scientific >>= \v -> pure (SffloatValue (realToFrac v))))
  <|> do
        a <- L.scientific
        b <- L.scientific
        c <- L.scientific
        d <- L.scientific
        return $ SfrotationValue (realToFrac a,realToFrac b,realToFrac c,realToFrac d)
  <|> do
        a <- L.scientific
        b <- L.scientific
        c <- L.scientific
        return $ Sfvec3fValue (realToFrac a,realToFrac b,realToFrac c)
  <|> do
        a <- L.scientific
        b <- L.scientific
        return $ Sfvec2fValue (realToFrac a,realToFrac b)

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
