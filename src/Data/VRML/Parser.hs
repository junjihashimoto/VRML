{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VRML.Parser
  ( Parser (..)
  , parseVRML
  ) where

import Data.VRML.Types
import GHC.Generics
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

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexm :: Parser a -> Parser a
lexm = L.lexeme sc


--space' = void $ takeWhileP (Just "white space") $ \t -> do
space' :: Parser String
space' = some $ oneOf [' ', '\t']

space'' :: Parser String
space'' = many $ oneOf [' ', '\t']

-- | parser of VRML
--
-- >>> parseTest parseVRML "#VRML_SIM R2020a utf8\nUSE hoge1"
-- VRML {version = "VRML_SIM R2020a utf8", statements = [StNode (USE (NodeNameId "hoge1"))]}
parseVRML :: Parser VRML
parseVRML = do
  version <- string "#" >> manyTill anySingle eol
  values <- some parseStatement
  return $ VRML version values

parseStatement :: Parser Statement
parseStatement = 
  (StRoute <$> parseRoute) <|>
  (StNode  <$> parseNodeStatement) <|>
  (StProto <$> parseProtoStatement)

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

-- | parser of Proto
--
-- >>> parseTest parseProtoStatement "PROTO Cube [] { Box {} }"
-- Proto (NodeTypeId "Cube") [] (ProtoBody [] (Node (NodeTypeId "Box") []) [])
parseProtoStatement :: Parser ProtoStatement
parseProtoStatement = 
  (id (Proto
   <$> (lstring "PROTO" >> parseNodeTypeId)
   <*> (lstring "[" >> many parseInterface >>= \v -> lstring "]" >> pure v )
   <*> (lstring "{" >> parseProtoBody >>= \v -> lstring "}" >> pure v))) <|>
  (id (ExternProto
   <$> (lstring "EXTERNPROTO" >> parseNodeTypeId)
   <*> (lstring "[" >> many parseExternInterface >>= \v -> lstring "]" >> pure v )
   <*> parseURLList))

-- | parser of ProtoBody
--
-- >>> parseTest parseProtoBody "hoge {}"
-- ProtoBody [] (Node (NodeTypeId "hoge") []) []
parseProtoBody :: Parser ProtoBody
parseProtoBody = do
  ProtoBody <$> many parseProtoStatement <*> parseNode <*> many parseStatement

parseRestrictedInterface :: Parser RestrictedInterface
parseRestrictedInterface =
  ( RestrictedInterfaceEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( RestrictedInterfaceEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( RestrictedInterfaceField <$> (lstring "field" >> parseFieldType) <*> parseFieldId <*> parseFieldValue)

parseInterface :: Parser Interface
parseInterface =
  ( InterfaceExposedField <$> (lstring "exposedField" >> parseFieldType) <*> parseFieldId <*> parseFieldValue) <|>
  ( Interface <$> parseRestrictedInterface)

parseExternInterface :: Parser ExternInterface
parseExternInterface =
  ( ExternInterfaceEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( ExternInterfaceEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( ExternInterfaceField <$> (lstring "field" >> parseFieldType) <*> parseFieldId) <|>
  ( ExternInterfaceExposedField <$> (lstring "exposedField" >> parseFieldType) <*> parseFieldId)

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

parseURLList :: Parser URLList
parseURLList =  
  (stringLiteral >>= \v -> pure (URLList [v])) <|>
  (lstring "[" >> many stringLiteral >>= \v -> lstring "]" >> pure (URLList v))

-- | parser of Node
--
-- >>> parseTest parseNode "hoge {hoge 1 hoge 2}"
-- Node (NodeTypeId "hoge") [NBFieldValue (FieldId "hoge") (SffloatValue 1.0),NBFieldValue (FieldId "hoge") (SffloatValue 2.0)]
-- >>> parseTest parseNode "BmwX5 { translation -78.7 0.4 7.53 }"
-- Node (NodeTypeId "BmwX5") [NBFieldValue (FieldId "translation") (Sfvec3fValue (-78.7,0.4,7.53))]
-- >>> parseTest parseNode "BmwX5 { rotation 0 1 0 1.5708}"
-- Node (NodeTypeId "BmwX5") [NBFieldValue (FieldId "rotation") (SfrotationValue (0.0,1.0,0.0,1.5708))]
-- >>> parseTest parseNode "BmwX5 { controller \"autonomous_vehicle\" }"
-- Node (NodeTypeId "BmwX5") [NBFieldValue (FieldId "controller") (SfstringValue "autonomous_vehicle")]
-- >>> parseTest parseNode "BmwX5 { translation -78.7 0.4 7.53  rotation 0 1 0 1.5708 controller \"autonomous_vehicle\" }"
-- Node (NodeTypeId "BmwX5") [NBFieldValue (FieldId "translation") (Sfvec3fValue (-78.7,0.4,7.53)),NBFieldValue (FieldId "rotation") (SfrotationValue (0.0,1.0,0.0,1.5708)),NBFieldValue (FieldId "controller") (SfstringValue "autonomous_vehicle")]
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

parseScriptBodyElement :: Parser ScriptBodyElement  
parseScriptBodyElement = 
  (SBEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId <*> (lstring "IS" >> parseEventInId) ) <|>
  (SBEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId <*> (lstring "IS" >> parseEventOutId) ) <|>
  (SBFieldId <$> (lstring "field" >> parseFieldType) <*> parseFieldId <*> (lstring "IS" >> parseFieldId)) <|>
  (SBRestrictedInterface <$> parseRestrictedInterface) <|>
  (SBNode <$> parseNodeBodyElement)

-- >>> parseTest parseBodyElement "maxPosition 1e4 1e4"
-- NBFieldValue (FieldId "maxPosition") (Sfvec2fValue (1e4,1e4))
-- >>> parseTest parseBodyElement "width IS width"
-- NBFieldId (FieldId "width") (FieldId "width")
parseNodeBodyElement :: Parser NodeBodyElement  
parseNodeBodyElement = 
  (NBRoute <$> parseRoute) <|>
  (NBProto <$> parseProtoStatement) <|>
  (try $ (NBFieldId <$> parseFieldId <*> (lstring "IS" >> parseFieldId) )) <|>
  (try $ (NBEeventIn <$> parseEventInId <*> (lstring "IS" >> parseEventInId) )) <|>
  (try $ (NBEeventOut <$> parseEventOutId <*> (lstring "IS" >> parseEventOutId) )) <|>
  (NBFieldValue <$> parseFieldId <*> parseFieldValue)

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
rws = ["PROTO","DEF","USE"]

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
  = (lstring "MFBool" >> pure MFBool)
  <|> (lstring "MFColor" >> pure MFColor)
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

parseFloat :: Parser Float
parseFloat =realToFrac <$> lexm pfloat

parseFloat' :: Parser Float
parseFloat' =realToFrac <$> pfloat

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
-- >>> parseTest parseFieldValue "[\n1e4 1e4\n1e4 1e4\n]"
-- Mfvec2fValue [(10000.0,10000.0),(10000.0,10000.0)]
-- >>> parseTest parseFieldValue "[\n1e4 1e4 1e4\n1e4 1e4 1e4\n]"
-- Mfvec3fValue [(10000.0,10000.0,10000.0),(10000.0,10000.0,10000.0)]
parseFieldValue :: Parser FieldValue
parseFieldValue
  =   (SfboolValue <$> parseBool)
  <|> (lstring "NULL" >> pure (SfnodeValue Nothing))
  <|> (try $ MfrotationValue <$> parseArrayN ((,,,)
                                              <$> parseFloat'
                                              <*> (space'' >> parseFloat')
                                              <*> (space'' >> parseFloat')
                                              <*> (space'' >> parseFloat')))
  <|> (try $ Mfvec3fValue <$> parseArrayN ((,,)
                                           <$> parseFloat'
                                           <*> (space'' >> parseFloat')
                                           <*> (space'' >> parseFloat')))
  <|> (try $ Mfvec2fValue <$> parseArrayN ((,)
                                           <$> parseFloat'
                                           <*> (space'' >> parseFloat')))
  <|> (try $ MffloatValue <$> parseArrayN parseFloat')
  <|> (try $ MfboolValue <$> parseArray' parseBool)
  <|> (try $ MfnodeValue <$> parseArray' parseNodeStatement)
  <|> (try $ MfstringValue <$> parseArray' stringLiteral)
  <|> (try $ MfrotationValue <$> parseArray ((,,,) <$> parseFloat <*> parseFloat <*> parseFloat <*> parseFloat))
  <|> (try $ Mfvec3fValue <$> parseArray ((,,) <$> parseFloat <*> parseFloat <*> parseFloat))
  <|> (try $ Mfvec2fValue <$> parseArray ((,) <$> parseFloat <*> parseFloat))
  <|> (try $ MffloatValue <$> parseArray parseFloat)
  <|> (try $ MfstringValue <$> parseArray stringLiteral)
  <|> (try $ MfboolValue <$> parseArray parseBool)
  <|> (try $ (\a b c d -> SfrotationValue (a,b,c,d)) <$> parseFloat <*> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b c -> Sfvec3fValue (a,b,c)) <$> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b -> Sfvec2fValue (a,b)) <$> parseFloat <*> parseFloat)
  <|> (try $ (SffloatValue <$> parseFloat))
  <|> (try $ (SfstringValue <$> stringLiteral))
  <|> (try $ (SfnodeValue . Just <$> parseNodeStatement))

parseArray :: Parser a -> Parser [a]
parseArray parser = do
  _ <- lstring "["
  values <-  parser `sepBy` lstring ","
  _ <- lstring "]"
  return values

parseArrayN :: Parser a -> Parser [a]
parseArrayN parser = do
  _ <- lstring "["
  values <-  some (try (space'' >> parser >>= \v -> space'' >> eol >> pure v))
  _ <- space'' >> lstring "]"
  return values

parseArray' :: Parser a -> Parser [a]
parseArray' parser = do
  _ <- lstring "["
  values <-  many parser
  _ <- lstring "]"
  return values

parseBool :: Parser Bool
parseBool
  =   (lstring "TRUE" >> pure True)
  <|> (lstring "FALSE" >> pure False)

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
