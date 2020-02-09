{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.VRML.Nodes where
import Data.VRML.Types
import GHC.Generics
import Data.Int

data Anchor = Anchor
  { children :: [Node]
  , description :: String
  , parameter :: [String]
  , url :: [String]
  , bboxCenter :: (Float,Float,Float)
  , bboxSize :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data Appearance = Appearance
  { material :: Maybe Node
  , texture :: Maybe Node
  , textureTransform :: Maybe Node
  } deriving (Generic,Show,Eq,ToNode)

data AudioClip = AudioClip
  { description :: String
  , loop :: Bool
  , pitch :: Float
  , startTime :: Time
  , stopTime :: Time
  , url :: [String]
  } deriving (Generic,Show,Eq,ToNode)

data Billboard = Billboard
  { axisOfRotation :: (Float,Float,Float)
  , children :: [Node]
  , bboxCenter :: (Float,Float,Float)
  , bboxSize :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data Box = Box
  { size :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data Collision = Collision
  { children :: [Node]
  , collide :: Bool
  , bboxCenter :: (Float,Float,Float)
  , bboxSize :: (Float,Float,Float)
  , proxy :: Maybe Node
  } deriving (Generic,Show,Eq,ToNode)

{-
data Color' = Color'
  { color :: [Color]
  } deriving (Generic,Show,Eq,ToNode)
-}

data ColorInterpolator = ColorInterpolator
  { key :: [Float]
  , keyValue :: [Color]
  } deriving (Generic,Show,Eq,ToNode)

data Cone = Cone
  { bottomRadius :: Float
  , height :: Float
  , side :: Bool
  , bottom :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data Coordinate = Coordinate
  { point :: [(Float,Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)

data CoordinateInterpolator = CoordinateInterpolator
  { key :: [Float]
  , keyValue :: [(Float,Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)

data Cylinder = Cylinder
  { bottom :: Bool
  , height :: Float
  , radius :: Float
  , side :: Bool
  , top :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data CylinderSensor = CylinderSensor
  { autoOffset :: Bool
  , diskAngle :: Float
  , enabled :: Bool
  , maxAngle :: Float
  , minAngle :: Float
  , offset :: Float
  } deriving (Generic,Show,Eq,ToNode)

data DirectionalLight = DirectionalLight
  { ambientIntensity :: Float
  , color :: Color
  , direction :: (Float,Float,Float)
  , intensity :: Float
  , on :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data ElevationGrid = ElevationGrid
  { color :: Maybe Node
  , normal :: Maybe Node
  , texCoord :: Maybe Node
  , height :: [Float]
  , ccw :: Bool
  , colorPerVertex :: Bool
  , creaseAngle :: Float
  , normalPerVertex :: Bool
  , solid :: Bool
  , xDimension :: Int32
  , xSpacing :: Float
  , zDimension :: Int32
  , zSpacing :: Float
  } deriving (Generic,Show,Eq,ToNode)

data Extrusion = Extrusion
  { beginCap :: Bool
  , ccw :: Bool
  , convex :: Bool
  , creaseAngle :: Float
  , crossSection :: [(Float,Float)]
  , endCap :: Bool
  , orientation :: [(Float,Float,Float,Float)]
  , scale :: [(Float,Float)]
  , solid :: Bool
  , spine :: [(Float,Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)

data Fog = Fog
  { color :: Color
  , fogType :: String
  , visibilityRange :: Float
  } deriving (Generic,Show,Eq,ToNode)

data FontStyle = FontStyle
  { family :: String
  , horizontal :: Bool
  , justify :: [String]
  , language :: String
  , leftToRight :: Bool
  , size :: Float
  , spacing :: Float
  , style :: String
  , topToBottom :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data Group = Group
  { children :: [Node]
  , bboxCenter :: (Float,Float,Float)
  , bboxSize :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data ImageTexture = ImageTexture
  { url :: [String]
  , repeatS :: Bool
  , repeatT :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data IndexedFaceSet = IndexedFaceSet
  { color :: Maybe Node
  , coord :: Maybe Node
  , normal :: Maybe Node
  , texCoord :: Maybe Node
  , ccw :: Bool
  , colorIndex :: [Int32]
  , colorPerVertex :: Bool
  , convex :: Bool
  , coordIndex :: [Int32]
  , creaseAngle :: Float
  , normalIndex :: [Int32]
  , normalPerVertex :: Bool
  , solid :: Bool
  , texCoordIndex :: [Int32]
  } deriving (Generic,Show,Eq,ToNode)

data IndexedLineSet = IndexedLineSet
  { color :: Maybe Node
  , coord :: Maybe Node
  , colorIndex :: [Int32]
  , colorPerVertex :: Bool
  , coordIndex :: [Int32]
  } deriving (Generic,Show,Eq,ToNode)

data Inline = Inline
  { url :: [String]
  , bboxCenter :: (Float,Float,Float)
  , bboxSize :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data LOD = LOD
  { level :: [Node]
  , center :: (Float,Float,Float)
  , range :: [Float]
  } deriving (Generic,Show,Eq,ToNode)

data Material = Material
  { ambientIntensity :: Float
  , diffuseColor :: Color
  , emissiveColor :: Color
  , shininess :: Float
  , specularColor :: Color
  , transparency :: Float
  } deriving (Generic,Show,Eq,ToNode)

data MovieTexture = MovieTexture
  { loop :: Bool
  , speed :: Float
  , startTime :: Time
  , stopTime :: Time
  , url :: [String]
  , repeatS :: Bool
  , repeatT :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data NavigationInfo = NavigationInfo
  { avatarSize :: [Float]
  , headlight :: Bool
  , speed :: Float
--  , type :: [String]
  , visibilityLimit :: Float
  } deriving (Generic,Show,Eq,ToNode)

data Normal = Normal
  { vector :: [(Float,Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)


data NormalInterpolator = NormalInterpolator
  { key :: [Float]
  , keyValue :: [(Float,Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)

data OrientationInterpolator = OrientationInterpolator
  { key :: [Float]
  , keyValue :: [(Float,Float,Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)

data PixelTexture = PixelTexture
  { image :: [Int32]
  , repeatS :: Bool
  , repeatT :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data PlaneSensor = PlaneSensor
  { autoOffset :: Bool
  , enabled :: Bool
  , maxPosition :: (Float,Float)
  , minPosition :: (Float,Float)
  , offset :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data PointLight = PointLight
  { ambientIntensity :: Float
  , attenuation :: (Float,Float,Float)
  , color :: Color
  , intensity :: Float
  , location :: (Float,Float,Float)
  , on :: Bool
  , radius :: Float
  } deriving (Generic,Show,Eq,ToNode)

data PointSet = PointSet
  { color :: Maybe Node
  , coord :: Maybe Node
  } deriving (Generic,Show,Eq,ToNode)

data PositionInterpolator = PositionInterpolator
  { key :: [Float]
  , keyValue :: [(Float,Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)

data ProximitySensor = ProximitySensor
  { center :: (Float,Float,Float)
  , size :: (Float,Float,Float)
  , enabled :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data ScalarInterpolator = ScalarInterpolator
  { key :: [Float]
  , keyValue :: [Float]
  } deriving (Generic,Show,Eq,ToNode)

data Shape = Shape
  { appearance :: Maybe Node
  , geometry :: Maybe Node
  } deriving (Generic,Show,Eq,ToNode)

data Sound = Sound
  { direction :: (Float,Float,Float)
  , intensity :: Float
  , location :: (Float,Float,Float)
  , maxBack :: Float
  , maxFront :: Float
  , minBack :: Float
  , minFront :: Float
  , priority :: Float
  , source :: Maybe Node
  , spatialize :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data Sphere = Sphere
  { radius :: Float
  } deriving (Generic,Show,Eq,ToNode)

data SphereSensor = SphereSensor
  { autoOffset :: Bool
  , enabled :: Bool
  , offset :: (Float,Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data SpotLight = SpotLight
  { ambientIntensity :: Float
  , attenuation :: (Float,Float,Float)
  , beamWidth :: Float
  , color :: Color
  , cutOffAngle :: Float
  , direction :: (Float,Float,Float)
  , intensity :: Float
  , location :: (Float,Float,Float)
  , on :: Bool
  , radius :: Float
  } deriving (Generic,Show,Eq,ToNode)

data Switch = Switch
  { choice :: [Node]
  , whichChoice :: Int32
  } deriving (Generic,Show,Eq,ToNode)

data Text = Text
  { string :: [String]
  , fontStyle :: Maybe Node
  , length :: [Float]
  , maxExtent :: Float
  } deriving (Generic,Show,Eq,ToNode)

data TextureCoordinate = TextureCoordinate
  { point :: [(Float,Float)]
  } deriving (Generic,Show,Eq,ToNode)

data TextureTransform = TextureTransform
  { center :: (Float,Float)
  , rotation :: Float
  , scale :: (Float,Float)
  , translation :: (Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data TimeSensor = TimeSensor
  { cycleInterval :: Time
  , enabled :: Bool
  , loop :: Bool
  , startTime :: Time
  , stopTime :: Time
  } deriving (Generic,Show,Eq,ToNode)

data TouchSensor = TouchSensor
  { enabled :: Bool
  } deriving (Generic,Show,Eq,ToNode)

data Transform = Transform
  { center :: (Float,Float,Float)
  , children :: [Node]
  , rotation :: (Float,Float,Float,Float)
  , scale :: (Float,Float,Float)
  , scaleOrientation :: (Float,Float,Float,Float)
  , translation :: (Float,Float,Float)
  , bboxCenter :: (Float,Float,Float)
  , bboxSize :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data Viewpoint = Viewpoint
  { fieldOfView :: Float
  , jump :: Bool
  , orientation :: (Float,Float,Float,Float)
  , position :: (Float,Float,Float)
  , description :: String
  } deriving (Generic,Show,Eq,ToNode)

data VisibilitySensor = VisibilitySensor
  { center :: (Float,Float,Float)
  , enabled :: Bool
  , size :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)

data WorldInfo = WorldInfo
  { info :: [String]
  , title :: String
  } deriving (Generic,Show,Eq,ToNode)

