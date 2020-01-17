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

data Statement
  = StatementNodeStatement NodeStatement
  | StatementProtoStatement ProtoStatement
  | StatementRouteStatement RouteStatement
  deriving (Show,Eq)

data NodeStatement
  = NodeStatementN Node
  | NodeStatementD NodeNameId Node
  | NodeStatementU NodeNameId
  deriving (Show,Eq)

data ProtoStatement
  = Proto NodeTypeId [InterfaceDeclaration] ProtoBody
  | ExternProto NodeTypeId [ExternInterfaceDeclaration] URLList
  deriving (Show,Eq)

data ProtoBody
  = ProtoBody [ProtoStatement] (Maybe NodeNameId) Node [Statement]
  deriving (Show,Eq)

data RestrictedInterfaceDeclaration
  = RestrictedInterfaceDeclarationEventIn FieldType EventInId
  | RestrictedInterfaceDeclarationEventOut FieldType EventOutId
  | RestrictedInterfaceDeclarationField FieldType FieldId FieldValue
  deriving (Show,Eq)

data InterfaceDeclaration
  = InterfaceDeclaration RestrictedInterfaceDeclaration
  | InterfaceDeclarationExposedField FieldType FieldId FieldValue
  deriving (Show,Eq)

data ExternInterfaceDeclaration
  = ExternInterfaceDeclarationEventIn FieldType EventInId
  | ExternInterfaceDeclarationEventOut FieldType EventOutId
  | ExternInterfaceDeclarationField FieldType FieldId
  | ExternInterfaceDeclarationExposedField FieldType FieldId
  deriving (Show,Eq)

data RouteStatement
  = RouteStatement NodeNameId EventOutId NodeNameId EventInId
  deriving (Show,Eq)

data URLList = URLList String
  deriving (Show,Eq)

data Node
  = Node NodeTypeId [NodeBodyElement]
  | Script [ScriptBodyElement]
  deriving (Show,Eq)

data ScriptBodyElement
  = ScriptBodyElementN NodeBodyElement
  | ScriptBodyElementR RestrictedInterfaceDeclaration
  | ScriptBodyElementEI FieldType EventInId EventInId
  | ScriptBodyElementEO FieldType EventOutId EventOutId
  | ScriptBodyElementF FieldType FieldId FieldId
  deriving (Show,Eq)

data NodeBodyElement
  = NodeBodyElementFV FieldId FieldValue
  | NodeBodyElementFI FieldId FieldId
  | NodeBodyElementEI EventInId EventInId
  | NodeBodyElementEO EventOutId EventOutId
  | NodeBodyElementR RouteStatement
  | NodeBodyElementP ProtoStatement
  deriving (Show,Eq)

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
parseFieldType :: Parser FieldType
parseFieldType
  =   (string "MFColor" >> pure MFColor)
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
