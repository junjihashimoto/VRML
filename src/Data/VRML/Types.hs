{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VRML.Types where

import GHC.Generics
import Data.Int
import Data.String

data VRML = VRML
  { version :: String
  , statements :: [Statement]
  }
  deriving (Generic,Show,Eq)

data Statement
  = StNode NodeStatement
  | StProto ProtoStatement
  | StRoute Route
  deriving (Generic,Eq)

class NodeLike a where
  node :: NodeTypeId -> [NodeBodyElement] -> a

instance NodeLike Statement where
  node i b = StNode (NodeStatement (Node i b))

instance NodeLike NodeStatement where
  node i b = NodeStatement (Node i b)

instance NodeLike Node where
  node i b = Node i b

instance NodeLike FieldValue where
  node i b = Snode (Just (NodeStatement (Node i b)))

instance Show Statement where
  show (StNode (NodeStatement (Node i b))) = "node " ++ show i ++ " " ++ show b
  show (StNode s) = "StNode (" ++ show s ++ ")"
  show (StProto s) = "StProto (" ++ show s
  show (StRoute s) = "StRoute (" ++ show s ++ ")"

data NodeStatement
  = NodeStatement Node
  | DEF NodeNameId Node
  | USE NodeNameId
  deriving (Generic,Show,Eq)

data ProtoStatement
  = Proto NodeTypeId [Interface] [ProtoStatement] Node [Statement]
  | ExternProto NodeTypeId [ExternInterface] URLList
  deriving (Generic,Show,Eq)

data RestrictedInterface
  = RestrictedInterfaceEventIn FieldType EventInId
  | RestrictedInterfaceEventOut FieldType EventOutId
  | RestrictedInterfaceField FieldType FieldId FieldValue
  deriving (Generic,Show,Eq)

data Interface
  = InterfaceEventIn FieldType EventInId
  | InterfaceEventOut FieldType EventOutId
  | InterfaceField FieldType FieldId FieldValue
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

newtype URLList = URLList [String]
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
  = FV FieldId FieldValue
  | NBFieldId FieldId FieldId
  | NBEventIn EventInId EventInId
  | NBEventOut EventOutId EventOutId
  | NBRoute Route
  | NBProto ProtoStatement
  deriving (Generic,Show,Eq)

newtype NodeNameId = NodeNameId String
  deriving (Generic,Show,Eq)

newtype NodeTypeId = NodeTypeId String
  deriving (Generic,Eq)

newtype FieldId = FieldId String
  deriving (Generic,Eq)

newtype EventInId = EventInId String
  deriving (Generic,Show,Eq)

newtype EventOutId = EventOutId String
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
  = Sbool Bool
  | Scolor (Float,Float,Float)
  | Sfloat Float
  | Simage [Int32]
  | Sint32 Int32
  | Snode (Maybe NodeStatement)
  | Srotation (Float,Float,Float,Float)
  | Sstring String
  | Stime Double
  | Svec2f (Float,Float)
  | Svec3f (Float,Float,Float)
  | Mbool [Bool]
  | Mcolor [(Float,Float,Float)]
  | Mfloat [Float]
  | Mint32 [Int32]
  | Mnode [NodeStatement]
  | Mrotation [(Float,Float,Float,Float)]
  | Mstring [String]
  | Mtime [Double]
  | Mvec2f [(Float,Float)]
  | Mvec3f [(Float,Float,Float)]
  deriving (Generic,Show,Eq)

instance IsString FieldValue where
  fromString s = Sstring s

instance IsString NodeNameId where
  fromString s = NodeNameId s

instance IsString NodeTypeId where
  fromString s = NodeTypeId s

instance IsString FieldId where
  fromString s = FieldId s

instance IsString EventInId where
  fromString s = EventInId s

instance IsString EventOutId where
  fromString s = EventOutId s

instance Show NodeTypeId where
  show (NodeTypeId s) = show s

instance Show FieldId where
  show (FieldId s) = show s

