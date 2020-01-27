{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VRML.Text where

import Data.VRML.Types
import GHC.Generics
import Data.Int
import Data.Void
import Control.Monad (void)
import Data.Char (isSpace)
import Data.Text hiding (empty, foldl, map)
import qualified Data.Text.Lazy.IO as TL
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

indent' = indent 2

instance Pretty VRML where
  pretty (VRML version statements) =
    header  <> vsep (map pretty statements)  <> line
    where
      header = "#" <> pretty version <> line

instance Pretty Statement where
  pretty (StNode v) = pretty v
  pretty (StProto v) = pretty v
  pretty (StRoute v) = pretty v

instance Pretty NodeStatement where
  pretty (DEF nodeNameId node) = "DEF" <+> pretty nodeNameId <+> pretty node
  pretty (USE nodeNameId) = "USE" <+> pretty nodeNameId
  pretty (NodeStatement node) = pretty node

instance Pretty ProtoStatement where
  pretty (Proto nodeTypeId interfaces ps node st) =
    vsep
    [ "PROTO" <+> pretty nodeTypeId <+> "["
    ,  indent' (vsep (map pretty interfaces))
    , "]"
    , "{"
    , indent'
      (vsep (
          (map (\v -> pretty v <> line ) ps) ++ 
            [pretty node] ++
            (map (\v -> pretty v <> line ) st)
          ))
    , "}"
    ]
  pretty (ExternProto nodeTypeId interfaces urllist) =
    vsep
    [ "EXTERNPROTO" <+> pretty nodeTypeId <+> "["
    ,  indent' (vsep (map pretty interfaces))
    , "]"
    , pretty urllist
    ]

instance Pretty RestrictedInterface where
  pretty (RestrictedInterfaceEventIn ft ei) =
    "eventIn" <+> pretty ft <+> pretty ei
  pretty (RestrictedInterfaceEventOut ft eo) =
    "eventOut" <+> pretty ft <+> pretty eo
  pretty (RestrictedInterfaceField ft fi fv) =
    "field" <+> pretty ft <+> pretty fi <+> pretty fv

instance Pretty Interface where
  pretty (InterfaceEventIn ft ei) =
    "eventIn" <+> pretty ft <+> pretty ei
  pretty (InterfaceEventOut ft eo) =
    "eventOut" <+> pretty ft <+> pretty eo
  pretty (InterfaceField ft fi fv) =
    "field" <+> pretty ft <+> pretty fi <+> pretty fv
  pretty (InterfaceExposedField ft fi fv) =
    "exposedField" <+> pretty ft <+> pretty fi <+> pretty fv

instance Pretty  ExternInterface where
  pretty (ExternInterfaceEventIn ft ei) =
    "eventIn" <+> pretty ft <+> pretty ei
  pretty (ExternInterfaceEventOut ft eo) =
    "eventOut" <+> pretty ft <+> pretty eo
  pretty (ExternInterfaceField ft fi) =
    "field" <+> pretty ft <+> pretty fi
  pretty (ExternInterfaceExposedField ft fi) =
    "exposedField" <+> pretty ft <+> pretty fi

instance Pretty Route where
  pretty (Route nidOut eo nidIn ei) =
    "ROUTE" <+> pretty nidOut <> "." <> pretty eo <+> pretty nidIn <> "." <> pretty ei

instance Pretty URLList where
  pretty (URLList urls) =
    vsep
    [ "["
    , indent' (vsep (map (\url -> pretty (Sstring url) <> line) urls))
    , "]"
    ]

instance Pretty Node where
  pretty (Node ntypeid bodys) =
    vsep
    [ pretty ntypeid <+> "{"
    , indent' (vsep (map (\v -> pretty v) bodys))
    , "}"
    ]
  pretty (Script bodys) =
    vsep
    [ "Script" <+> "{"
    , indent' (vsep (map (\v -> pretty v) bodys))
    , "}"
    ]

instance Pretty ScriptBodyElement where
  pretty (SBNode v) = pretty v
  pretty (SBRestrictedInterface v) = pretty v
  pretty (SBEventIn etype eid1 eid2) = "eventIn" <+> pretty etype <+> pretty eid1 <+> "IS" <+>pretty eid2
  pretty (SBEventOut etype eid1 eid2) = "eventOut" <+> pretty etype <+> pretty eid1 <+> "IS" <+>pretty eid2
  pretty (SBFieldId etype eid1 eid2) = "field" <+> pretty etype <+> pretty eid1 <+> "IS" <+>pretty eid2

instance Pretty NodeBodyElement where
  pretty (FV fid fv) = pretty fid <+> pretty fv
  pretty (NBFieldId fid1 fid2) = pretty fid1 <+> pretty fid2
  pretty (NBEventIn eid1 eid2) = pretty eid1 <+> "IS" <+>pretty eid2
  pretty (NBEventOut eid1 eid2) = pretty eid1 <+> "IS" <+>pretty eid2
  pretty (NBRoute r) = pretty r
  pretty (NBProto p) = pretty p 

instance Pretty NodeNameId where
  pretty (NodeNameId str) = pretty str

instance Pretty NodeTypeId where
  pretty (NodeTypeId str) = pretty str

instance Pretty FieldId where
  pretty (FieldId str) = pretty str

instance Pretty EventInId where
  pretty (EventInId str) = pretty str

instance Pretty EventOutId where
  pretty (EventOutId str) = pretty str

instance Pretty FieldType where
  pretty MFBool = "MFBool"
  pretty MFColor = "MFColor"
  pretty MFFloat = "MFFloat"
  pretty MFString = "MFString"
  pretty MFTime = "MFTime"
  pretty MFVec2f = "MFVec2f"
  pretty MFVec3f = "MFVec3f"
  pretty SFBool = "SFBool"
  pretty SFColor = "SFColor"
  pretty SFFloat = "SFFloat"
  pretty SFImage = "SFImage"
  pretty SFInt32 = "SFInt32"
  pretty SFNode = "SFNode"
  pretty SFRotation = "SFRotation"
  pretty SFString = "SFString"
  pretty SFTime = "SFTime"
  pretty SFVec2f = "SFVec2f"
  pretty SFVec3f = "SFVec3f"

instance Pretty FieldValue where
  pretty (Sbool True) = "TRUE"
  pretty (Sbool False) = "FALSE"
  pretty (Scolor (v1,v2,v3)) = pretty v1 <+> pretty v2 <+> pretty v3
  pretty (Sfloat v) = pretty v
  pretty (Simage v) = foldl (<+>) "[" (map pretty v) <+> "]"
  pretty (Sint32 v) = pretty v
  pretty (Snode (Just v)) = pretty v
  pretty (Snode Nothing) = "NULL"
  pretty (Srotation (v1,v2,v3,v4)) = pretty v1 <+> pretty v2 <+> pretty v3 <+> pretty v4
  pretty (Sstring v) =
    let rep [] = []
        rep ('"' : xs) = '\\' : '"' : rep xs
        rep (x : xs) = x : rep xs
    in "\"" <> pretty(rep v) <> "\"" 

  pretty (Stime v) = pretty v
  pretty (Svec2f (v1,v2)) = pretty v1 <+> pretty v2
  pretty (Svec3f (v1,v2,v3)) = pretty v1 <+> pretty v2 <+> pretty v3
  pretty (Mbool vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Sbool v)) vs))
    , "]"
    ]
  pretty (Mcolor vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Scolor v)) vs))
    , "]"
    ]
  pretty (Mfloat vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Sfloat v)) vs))
    , "]"
    ]
  pretty (Mint32 vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Sint32 v)) vs))
    , "]"
    ]
  pretty (Mnode vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Snode (Just v))) vs))
    , "]"
    ]
  pretty (Mrotation vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Srotation v)) vs))
    , "]"
    ]
  pretty (Mstring vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Sstring v)) vs))
    , "]"
    ]
  pretty (Mtime vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Stime v)) vs))
    , "]"
    ]
  pretty (Mvec2f vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Svec2f v)) vs))
    , "]"
    ]
  pretty (Mvec3f vs) =
    vsep
    [ "["
    , indent' (vsep (map (\v -> pretty (Svec3f v)) vs))
    , "]"
    ]


writeVRML :: FilePath -> VRML ->  IO ()
writeVRML filename doc =
  TL.writeFile filename $ renderLazy $ layoutPretty defaultLayoutOptions (pretty doc)
