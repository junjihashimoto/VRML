{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.VRML.Nodes where
import Data.VRML.Types
import GHC.Generics

data Box = Box
  { size :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode,ToNodeLike)

data Sphere = Sphere
  { radius :: Float
  } deriving (Generic,Show,Eq,ToNode,ToNodeLike)

data Cylinder = Cylinder
  { bottom :: Bool
  , height :: Float
  , radius :: Float
  , side :: Bool
  , top :: Bool
  } deriving (Generic,Show,Eq,ToNode,ToNodeLike)

data Shape = Shape
  { appearance :: Maybe Node
  , geometry   :: Maybe Node
  } deriving (Generic,Show,Eq,ToNode,ToNodeLike)

data DirectionalLight = DirectionalLight
  { ambientIntensity :: Float
  , color :: Color
  , direction :: (Float,Float,Float)
  , intensity :: Float
  , on :: Bool
  } deriving (Generic,Show,Eq,ToNode,ToNodeLike)
