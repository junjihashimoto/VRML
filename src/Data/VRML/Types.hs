{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

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

newtype Color = Color (Float,Float,Float) deriving (Generic,Show,Eq)
newtype Time = Time Double deriving (Generic,Show,Eq)

data FieldValue
  = Sbool Bool
  | Scolor Color
  | Sfloat Float
  | Simage [Int32]
  | Sint32 Int32
  | Snode (Maybe NodeStatement)
  | Srotation (Float,Float,Float,Float)
  | Sstring String
  | Stime Time
  | Svec2f (Float,Float)
  | Svec3f (Float,Float,Float)
  | Mbool [Bool]
  | Mcolor [Color]
  | Mfloat [Float]
  | Mint32 [Int32]
  | Mnode [NodeStatement]
  | Mrotation [(Float,Float,Float,Float)]
  | Mstring [String]
  | Mtime [Time]
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


instance Semigroup Node where
  (<>) a b = 
    let (Node (NodeTypeId xname) xbody) = a
        (Node (NodeTypeId yname) ybody) = b
    in Node (NodeTypeId (xname ++ yname)) (xbody++ybody)

instance Monoid Node where
  mempty = Node "" []

class ToNode a where
  toNode :: a -> Node
  default toNode :: (Generic a, ToNode' (Rep a)) => a -> Node
  toNode a = toNode' (from a)

instance ToNode Bool where
  toNode a = Node "" [(FV "" (Sbool a))]

instance ToNode Color where
  toNode a = Node "" [(FV "" (Scolor a))]

instance ToNode Float where
  toNode a = Node "" [(FV "" (Sfloat a))]

instance ToNode Int32 where
  toNode a = Node "" [(FV "" (Sint32 a))]

instance ToNode Node where
  toNode a = Node "" [(FV "" (Snode (Just (NodeStatement a))))]

instance ToNode (Float,Float,Float,Float) where
  toNode a = Node "" [(FV "" (Srotation a))]

instance ToNode String where
  toNode a = Node "" [(FV "" (Sstring a))]

instance ToNode Time where
  toNode a = Node "" [(FV "" (Stime a))]

instance ToNode (Float,Float) where
  toNode a = Node "" [(FV "" (Svec2f a))]

instance ToNode (Float,Float,Float) where
  toNode a = Node "" [(FV "" (Svec3f a))]

instance ToNode [Bool] where
  toNode a = Node "" [(FV "" (Mbool a))]

instance ToNode [Color] where
  toNode a = Node "" [(FV "" (Mcolor a))]

instance ToNode [Float] where
  toNode a = Node "" [(FV "" (Mfloat a))]

instance ToNode [Int32] where
  toNode a = Node "" [(FV "" (Mint32 a))]

instance ToNode [Node] where
  toNode a = Node "" [(FV "" (Mnode (map NodeStatement a)))]

instance ToNode [(Float,Float,Float,Float)] where
  toNode a = Node "" [(FV "" (Mrotation a))]

instance ToNode [Time] where
  toNode a = Node "" [(FV "" (Mtime a))]

instance ToNode [String] where
  toNode a = Node "" [(FV "" (Mstring a))]

instance ToNode [(Float,Float)] where
  toNode a = Node "" [(FV "" (Mvec2f a))]

instance ToNode [(Float,Float,Float)] where
  toNode a = Node "" [(FV "" (Mvec3f a))]

class ToNode' f where
  toNode' :: f a -> Node

instance ToNode' U1 where
  toNode' U1 = mempty

instance (ToNode' f, ToNode' g) => ToNode' (f :+: g) where
  toNode' (L1 x) = toNode' x
  toNode' (R1 x) = toNode' x

instance (ToNode' f, ToNode' g) => ToNode' (f :*: g) where
  toNode' (x :*: y) = toNode' x <> toNode' y

instance (ToNode c) => ToNode' (K1 i c) where
  toNode' (K1 x) = toNode x

instance (Selector c, ToNode' f) => ToNode' (M1 S c f) where
  toNode' a@(M1 x) =
    case toNode' x of
      (Node "" [(FV _ v)]) -> Node "" [(FV (FieldId (selName a)) v)]
      v@(Node x _) -> Node "" [(FV (FieldId (selName a)) (Snode (Just (NodeStatement v))))]

instance (Constructor c, ToNode' f) => ToNode' (M1 C c f) where
  toNode' a@(M1 x) = Node (NodeTypeId (conName a)) [] <> toNode' x

instance (ToNode' f) => ToNode' (M1 D c f) where
  toNode' (M1 x) = toNode' x
