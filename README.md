# VRML

VRML is a text file format for a 3D polygon.
It is a standard known as ISO/IEC 14772-1:1997.
It has been superseded by X3D.

[webots](https://cyberbotics.com/) uses VRML-format to make simulation environment.
This package is developed for making the environment by haskell.

# VRML to Haskell

vrml2haskell command generates haskell-code from VRML-file.
Usage is below.

```
> vrml2haskell "vrml file" > "haskell file"
```

# Haskell to VRML

'ToNode' type-class makes VRML-data from Haskell-data with deriving ToNode.

```
class ToNode a where
  toNode :: NodeLike b => a -> b
```

Usage of deriving ToNode is below.

```
data Box = Box
  { size :: (Float,Float,Float)
  } deriving (Generic,Show,Eq,ToNode)
```

