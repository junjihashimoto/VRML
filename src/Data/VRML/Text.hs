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
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer as L
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

instance Pretty VRML where
  pretty (VRML version statements) =
    header  <> vsep (map pretty statements)
    where
      header = "#" <> pretty version <> line

instance Pretty Statement where
  pretty (StNode v) = pretty v
  pretty (StProto v) = pretty v
  pretty (StRoute v) = pretty v

instance Pretty NodeStatement where
  pretty (DEF nodeNameId node) = "DEF" <+> pretty nodeNameId <+> pretty node
  pretty (USE nodeNameId) = "USE" <+> pretty nodeNameId <> line
  pretty (NodeStatement node) = pretty node

instance Pretty ProtoStatement where
  pretty (Proto nodeTypeId interfaces protoBody) =
    "PROTO" <+> pretty nodeTypeId <> line <>
    "[" <> line <>
    nest 4 (vsep (map pretty interfaces)) <>
    "]" <> line <>
    "{" <> line <>
    nest 4 (pretty protoBody) <>
    "}" <> line  
  pretty (ExternProto nodeTypeId interfaces urllist) =
    "EXTERNPROTO" <+> pretty nodeTypeId <> line <>
    "[" <> line <>
    nest 4 (vsep (map pretty interfaces)) <>
    "]" <> line <>
    pretty urllist

instance Pretty ProtoBody where
  pretty (ProtoBody ps node st) =
    vsep (map (\v -> pretty v <> line ) ps) <>
    pretty node <>
    vsep (map (\v -> pretty v <> line ) st)

instance Pretty RestrictedInterface where
  pretty (RestrictedInterfaceEventIn ft ei) =
    "eventIn" <+> pretty ft <+> pretty ei
  pretty (RestrictedInterfaceEventOut ft eo) =
    "eventOut" <+> pretty ft <+> pretty eo
  pretty (RestrictedInterfaceField ft fi fv) =
    "field" <+> pretty ft <+> pretty fi <+> pretty fv

instance Pretty Interface where
  pretty (Interface ri) = pretty ri 
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
    "[" <> line <> 
    nest 4 (vsep $ map (\url -> pretty (SfstringValue url) <> line) urls) <>
    "]" <> line

instance Pretty Node where
  pretty (Node ntypeid bodys) =
    pretty ntypeid <+> "{" <> line <>
    nest 4 (vsep $ map (\v -> pretty v <> line) bodys) <>
    "}" <> line
  pretty (Script bodys) =
    "Script" <+> "{" <> line <>
    nest 4 (vsep $ map (\v -> pretty v <> line) bodys) <>
    "}" <> line

instance Pretty ScriptBodyElement where
  pretty (SBNode v) = pretty v
  pretty (SBRestrictedInterface v) = pretty v
  pretty (SBEventIn etype eid1 eid2) = "eventIn" <+> pretty etype <+> pretty eid1 <+> "IS" <+>pretty eid2
  pretty (SBEventOut etype eid1 eid2) = "eventOut" <+> pretty etype <+> pretty eid1 <+> "IS" <+>pretty eid2
  pretty (SBFieldId etype eid1 eid2) = "field" <+> pretty etype <+> pretty eid1 <+> "IS" <+>pretty eid2

instance Pretty NodeBodyElement where
  pretty (NBFieldValue fid fv) = pretty fid <+> pretty fv
  pretty (NBFieldId fid1 fid2) = pretty fid1 <+> pretty fid2
  pretty (NBEeventIn eid1 eid2) = pretty eid1 <+> "IS" <+>pretty eid2
  pretty (NBEeventOut eid1 eid2) = pretty eid1 <+> "IS" <+>pretty eid2
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
  pretty (SfboolValue True) = "TRUE"
  pretty (SfboolValue False) = "FALSE"
  pretty (SfcolorValue (v1,v2,v3)) = pretty v1 <+> pretty v2 <+> pretty v3
  pretty (SffloatValue v) = pretty v
  pretty (SfimageValue v) = foldl (<+>) "[" (map pretty v) <+> "]"
  pretty (Sfint32Value v) = pretty v
  pretty (SfnodeValue (Just v)) = pretty v
  pretty (SfnodeValue Nothing) = "NULL"
  pretty (SfrotationValue (v1,v2,v3,v4)) = pretty v1 <+> pretty v2 <+> pretty v3 <+> pretty v4
  pretty (SfstringValue v) =
    let rep [] = []
        rep ('"' : xs) = '\\' : '"' : rep xs
        rep (x : xs) = x : rep xs
    in "\"" <> pretty(rep v) <> "\"" 

  pretty (SftimeValue v) = pretty v
  pretty (Sfvec2fValue (v1,v2)) = pretty v1 <+> pretty v2
  pretty (Sfvec3fValue (v1,v2,v3)) = pretty v1 <+> pretty v2 <+> pretty v3
  pretty (MfboolValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (SfboolValue v) <> line) vs)
    <> "]"
  pretty (MfcolorValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (SfcolorValue v) <> line) vs)
    <> "]"
  pretty (MffloatValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (SffloatValue v) <> line) vs)
    <> "]"
  pretty (Mfint32Value vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (Sfint32Value v) <> line) vs)
    <> "]"
  pretty (MfnodeValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (SfnodeValue (Just v)) <> line) vs)
    <> "]"
  pretty (MfrotationValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (SfrotationValue v) <> line) vs)
    <> "]"
  pretty (MfstringValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (SfstringValue v) <> line) vs)
    <> "]"
  pretty (MftimeValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (SftimeValue v) <> line) vs)
    <> "]"
  pretty (Mfvec2fValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (Sfvec2fValue v) <> line) vs)
    <> "]"
  pretty (Mfvec3fValue vs) =
    "[" <> line <>
    nest 4 (vsep $ map (\v -> pretty (Sfvec3fValue v) <> line) vs)
    <> "]"

