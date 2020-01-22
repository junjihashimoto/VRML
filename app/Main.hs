{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)
  parseTest parseVrmlScene f
  --putDoc $ pretty $
  --  Node (NodeTypeId "hoge") []
