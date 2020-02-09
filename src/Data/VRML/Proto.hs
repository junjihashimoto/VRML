{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VRML.Proto where

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
  pretty (VRML _ statements) =
    vsep (map pretty statements)  <> line

instance Pretty Statement where
  pretty (StNode v) = ""
  pretty (StProto v) = pretty v
  pretty (StRoute v) = ""

instance Pretty NodeStatement where
  pretty (DEF nodeNameId node) = ""
  pretty (USE nodeNameId) = ""
  pretty (NodeStatement node) = ""

instance Pretty ProtoStatement where
  pretty (Proto nodeTypeId [] _ _ _) =
    vsep
    [ "data" <+> pretty nodeTypeId <+> "=" <+> pretty nodeTypeId <+> "deriving (Generic,Show,Eq,ToNode)"
    ] <> line
  pretty (Proto nodeTypeId (interface:interfaces) _ _ _) =
    vsep
    [ "data" <+> pretty nodeTypeId <+> "=" <+> pretty nodeTypeId
    , indent' ("{" <+> pretty interface)
    , indent' (vsep (map (\i -> "," <+> pretty i) interfaces))
    , indent' ("}" <+> "deriving (Generic,Show,Eq,ToNode)")
    ] <> line
  pretty (ExternProto nodeTypeId interfaces _) = ""


instance Pretty RestrictedInterface where
  pretty (RestrictedInterfaceEventIn ft ei) = ""
  pretty (RestrictedInterfaceEventOut ft eo) = ""
  pretty (RestrictedInterfaceField ft fi fv) =
    pretty fi <+> "::" <+> pretty ft

instance Pretty Interface where
  pretty (InterfaceEventIn ft ei) = ""
  pretty (InterfaceEventOut ft eo) = ""
  pretty (InterfaceField ft fi fv) =
    pretty fi <+> "::" <+> pretty ft
  pretty (InterfaceExposedField ft fi fv) =
    pretty fi <+> "::" <+> pretty ft

instance Pretty  ExternInterface where
  pretty (ExternInterfaceEventIn ft ei) = ""
  pretty (ExternInterfaceEventOut ft eo) = ""
  pretty (ExternInterfaceField ft fi) =
    pretty fi <+> "::" <+> pretty ft
  pretty (ExternInterfaceExposedField ft fi) =
    pretty fi <+> "::" <+> pretty ft

instance Pretty Route where
  pretty (Route nidOut eo nidIn ei) = ""

instance Pretty URLList where
  pretty (URLList urls) = ""

instance Pretty Node where
  pretty _ = ""

instance Pretty ScriptBodyElement where
  pretty _ = ""

instance Pretty NodeBodyElement where
  pretty _ = ""

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
  pretty MFNode = "[Node]"
  pretty MFBool = "[Bool]"
  pretty MFColor = "[Color]"
  pretty MFFloat = "[Float]"
  pretty MFString = "[String]"
  pretty MFTime = "[Time]"
  pretty MFVec2f = "[(Float,Float)]"
  pretty MFVec3f = "[(Float,Float,Float)]"
  pretty MFInt32 = "[Int32]"
  pretty MFRotation = "[(Float,Float,Float,Float)]"
  pretty SFBool = "Bool"
  pretty SFColor = "Color"
  pretty SFFloat = "Float"
  pretty SFImage = "[Int32]"
  pretty SFInt32 = "Int32"
  pretty SFNode = "Maybe Node"
  pretty SFRotation = "(Float,Float,Float,Float)"
  pretty SFString = "String"
  pretty SFTime = "Time"
  pretty SFVec2f = "(Float,Float)"
  pretty SFVec3f = "(Float,Float,Float)"

instance Pretty FieldValue where
  pretty _ = ""

writeHaskell :: FilePath -> VRML ->  IO ()
writeHaskell filename doc =
  TL.writeFile filename $ renderLazy $ layoutPretty defaultLayoutOptions (pretty doc)
