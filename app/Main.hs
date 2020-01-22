{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.VRML
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)
  parseTest parseVRML f
  --putDoc $ pretty $
  --  Node (NodeTypeId "hoge") []
