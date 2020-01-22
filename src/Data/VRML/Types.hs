{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VRML.Types where


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

data VRML = VRML
  { version :: String
  , statements :: [Statement]
  }
  deriving (Generic,Show,Eq)

data Statement
  = StNode NodeStatement
  | StProto ProtoStatement
  | StRoute Route
  deriving (Generic,Show,Eq)

data NodeStatement
  = NodeStatement Node
  | DEF NodeNameId Node
  | USE NodeNameId
  deriving (Generic,Show,Eq)

data ProtoStatement
  = Proto NodeTypeId [Interface] ProtoBody
  | ExternProto NodeTypeId [ExternInterface] URLList
  deriving (Generic,Show,Eq)

data ProtoBody
  = ProtoBody [ProtoStatement] Node [Statement]
  deriving (Generic,Show,Eq)

data RestrictedInterface
  = RestrictedInterfaceEventIn FieldType EventInId
  | RestrictedInterfaceEventOut FieldType EventOutId
  | RestrictedInterfaceField FieldType FieldId FieldValue
  deriving (Generic,Show,Eq)

data Interface
  = Interface RestrictedInterface
  | InterfaceExposedField FieldType FieldId FieldValue
  deriving (Generic,Show,Eq)

data ExternInterface
  = ExternInterfaceEventIn FieldType EventInId
  | ExternInterfaceEventOut FieldType EventOutId
  | ExternInterfaceField FieldType FieldId
  | ExternInterfaceExposedField FieldType FieldId
  deriving (Generic,Show,Eq)

data Route
  = Route NodeNameId EventOutId NodeNameId EventInId
  deriving (Generic,Show,Eq)

data URLList = URLList [String]
  deriving (Generic,Show,Eq)

data Node
  = Node NodeTypeId [NodeBodyElement]
  | Script [ScriptBodyElement]
  deriving (Generic,Show,Eq)

data ScriptBodyElement
  = SBNode NodeBodyElement
  | SBRestrictedInterface RestrictedInterface
  | SBEventIn FieldType EventInId EventInId
  | SBEventOut FieldType EventOutId EventOutId
  | SBFieldId FieldType FieldId FieldId
  deriving (Generic,Show,Eq)

data NodeBodyElement
  = NBFieldValue FieldId FieldValue
  | NBFieldId FieldId FieldId
  | NBEeventIn EventInId EventInId
  | NBEeventOut EventOutId EventOutId
  | NBRoute Route
  | NBProto ProtoStatement
  deriving (Generic,Show,Eq)

data NodeNameId = NodeNameId String
  deriving (Generic,Show,Eq)

data NodeTypeId = NodeTypeId String
  deriving (Generic,Show,Eq)

data FieldId = FieldId String
  deriving (Generic,Show,Eq)

data EventInId = EventInId String
  deriving (Generic,Show,Eq)

data EventOutId = EventOutId String
  deriving (Generic,Show,Eq)

data FieldType
  = MFBool
  | MFColor
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
  deriving (Generic,Show,Eq)

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
  | MfboolValue [Bool]
  | MfcolorValue [(Float,Float,Float)]
  | MffloatValue [Float]
  | Mfint32Value [Int32]
  | MfnodeValue [NodeStatement]
  | MfrotationValue [(Float,Float,Float,Float)]
  | MfstringValue [String]
  | MftimeValue [Double]
  | Mfvec2fValue [(Float,Float)]
  | Mfvec3fValue [(Float,Float,Float)]
  deriving (Generic,Show,Eq)
