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
-- NodeStatement (Node "hoge" [])
-- >>> parseTest parseNodeStatement "DEF hoge1 hoge {}"
-- DEF (NodeNameId "hoge1") (Node "hoge" [])
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
-- Proto "Cube" [] [] (Node "Box" []) []
parseProtoStatement :: Parser ProtoStatement
parseProtoStatement = 
  (id (Proto
   <$> (lstring "PROTO" >> parseNodeTypeId)
   <*> (lstring "[" >> many parseInterface <* lstring "]")
   <*> (lstring "{" >> many parseProtoStatement)
   <*> parseNode
   <*> (many parseStatement <* lstring "}"))) <|>
  (id (ExternProto
   <$> (lstring "EXTERNPROTO" >> parseNodeTypeId)
   <*> (lstring "[" >> many parseExternInterface <* lstring "]")
   <*> parseURLList))

parseRestrictedInterface :: Parser RestrictedInterface
parseRestrictedInterface =
  ( RestrictedInterfaceEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( RestrictedInterfaceEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( RestrictedInterfaceField <$> (lstring "field" >> parseFieldType) <*> parseFieldId <*> parseFieldValue)

parseInterface :: Parser Interface
parseInterface =
  ( InterfaceEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId) <|>
  ( InterfaceEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId) <|>
  ( InterfaceField <$> (lstring "field" >> parseFieldType) <*> parseFieldId <*> parseFieldValue) <|>
  ( InterfaceExposedField <$> (lstring "exposedField" >> parseFieldType) <*> parseFieldId <*> parseFieldValue)

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
  ((\v -> URLList [v]) <$> stringLiteral) <|>
  (URLList <$> (lstring "[" >> many stringLiteral <* lstring "]"))

-- | parser of Node
--
-- >>> parseTest parseNode "hoge {hoge 1 hoge 2}"
-- Node "hoge" [FV "hoge" (Sfloat 1.0),FV "hoge" (Sfloat 2.0)]
-- >>> parseTest parseNode "BmwX5 { translation -78.7 0.4 7.53 }"
-- Node "BmwX5" [FV "translation" (Svec3f (-78.7,0.4,7.53))]
-- >>> parseTest parseNode "BmwX5 { rotation 0 1 0 1.5708}"
-- Node "BmwX5" [FV "rotation" (Srotation (0.0,1.0,0.0,1.5708))]
-- >>> parseTest parseNode "BmwX5 { controller \"autonomous_vehicle\" }"
-- Node "BmwX5" [FV "controller" (Sstring "autonomous_vehicle")]
-- >>> parseTest parseNode "BmwX5 { translation -78.7 0.4 7.53  rotation 0 1 0 1.5708 controller \"autonomous_vehicle\" }"
-- Node "BmwX5" [FV "translation" (Svec3f (-78.7,0.4,7.53)),FV "rotation" (Srotation (0.0,1.0,0.0,1.5708)),FV "controller" (Sstring "autonomous_vehicle")]
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

parseScriptBodyElement :: Parser ScriptBodyElement  
parseScriptBodyElement = 
  (SBEventIn <$> (lstring "eventIn" >> parseFieldType) <*> parseEventInId <*> (lstring "IS" >> parseEventInId) ) <|>
  (SBEventOut <$> (lstring "eventOut" >> parseFieldType) <*> parseEventOutId <*> (lstring "IS" >> parseEventOutId) ) <|>
  (SBFieldId <$> (lstring "field" >> parseFieldType) <*> parseFieldId <*> (lstring "IS" >> parseFieldId)) <|>
  (SBRestrictedInterface <$> parseRestrictedInterface) <|>
  (SBNode <$> parseNodeBodyElement)

-- >>> parseTest parseBodyElement "maxPosition 1e4 1e4"
-- FV "maxPosition" (Svec2f (1e4,1e4))
-- >>> parseTest parseBodyElement "width IS width"
-- NBFieldId "width" "width"
parseNodeBodyElement :: Parser NodeBodyElement  
parseNodeBodyElement = 
  (NBRoute <$> parseRoute) <|>
  (NBProto <$> parseProtoStatement) <|>
  (try $ (NBFieldId <$> parseFieldId <*> (lstring "IS" >> parseFieldId) )) <|>
  (try $ (NBEventIn <$> parseEventInId <*> (lstring "IS" >> parseEventInId) )) <|>
  (try $ (NBEventOut <$> parseEventOutId <*> (lstring "IS" >> parseEventOutId) )) <|>
  (FV <$> parseFieldId <*> parseFieldValue)

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
  <|> (lstring "MFNode" >> pure MFNode)
  <|> (lstring "MFRotation" >> pure MFRotation)
  <|> (lstring "MFInt32" >> pure MFInt32)
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
-- Sbool True
-- >>> parseTest parseFieldValue "FALSE"
-- Sbool False
-- >>> parseTest parseFieldValue "NULL"
-- Snode Nothing
-- >>> parseTest parseFieldValue "\"hoge\\\"hoge\""
-- Sstring "hoge\"hoge"
-- >>> parseTest parseFieldValue "\"autonomous_vehicle\""
-- Sstring "autonomous_vehicle"
-- >>> parseTest parseFieldValue "1e4 1e4"
-- Svec2f (10000.0,10000.0)
-- >>> parseTest parseFieldValue "[1e4 1e4 1e4,1e4 1e4 1e4]"
-- Mvec3f [(10000.0,10000.0,10000.0),(10000.0,10000.0,10000.0)]
-- >>> parseTest parseFieldValue "[\n1e4 1e4\n1e4 1e4\n]"
-- Mvec2f [(10000.0,10000.0),(10000.0,10000.0)]
-- >>> parseTest parseFieldValue "[\n1e4 1e4 1e4\n1e4 1e4 1e4\n]"
-- Mvec3f [(10000.0,10000.0,10000.0),(10000.0,10000.0,10000.0)]
parseFieldValue :: Parser FieldValue
parseFieldValue
  =   (Sbool <$> parseBool)
  <|> (lstring "NULL" >> pure (Snode Nothing))
  <|> (try $ Mrotation <$> parseArrayN ((,,,)
                                              <$> parseFloat'
                                              <*> (space'' >> parseFloat')
                                              <*> (space'' >> parseFloat')
                                              <*> (space'' >> parseFloat')))
  <|> (try $ Mvec3f <$> parseArrayN ((,,)
                                           <$> parseFloat'
                                           <*> (space'' >> parseFloat')
                                           <*> (space'' >> parseFloat')))
  <|> (try $ Mvec2f <$> parseArrayN ((,)
                                           <$> parseFloat'
                                           <*> (space'' >> parseFloat')))
  <|> (try $ Mfloat <$> parseArrayN parseFloat')
  <|> (try $ Mbool <$> parseArray' parseBool)
  <|> (try $ Mnode <$> parseArray' parseNodeStatement)
  <|> (try $ Mstring <$> parseArray' stringLiteral)
  <|> (try $ Mrotation <$> parseArray ((,,,) <$> parseFloat <*> parseFloat <*> parseFloat <*> parseFloat))
  <|> (try $ Mvec3f <$> parseArray ((,,) <$> parseFloat <*> parseFloat <*> parseFloat))
  <|> (try $ Mvec2f <$> parseArray ((,) <$> parseFloat <*> parseFloat))
  <|> (try $ Mfloat <$> parseArray parseFloat)
  <|> (try $ Mstring <$> parseArray stringLiteral)
  <|> (try $ Mbool <$> parseArray parseBool)
  <|> (try $ (\a b c d -> Srotation (a,b,c,d)) <$> parseFloat <*> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b c -> Svec3f (a,b,c)) <$> parseFloat <*> parseFloat <*> parseFloat)
  <|> (try $ (\a b -> Svec2f (a,b)) <$> parseFloat <*> parseFloat)
  <|> (try $ (Sfloat <$> parseFloat))
  <|> (try $ (Sstring <$> stringLiteral))
  <|> (try $ (Snode . Just <$> parseNodeStatement))

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
