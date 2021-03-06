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
import Control.Monad (void)
  
main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)
  case parse parseVRML "" f of
    Right v -> putDoc $ pretty v
    Left _ -> void $ parseTest parseVRML f
