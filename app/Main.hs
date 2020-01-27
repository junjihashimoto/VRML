{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.VRML
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import Text.Show.Prettyprint (prettyPrint,prettifyToDoc)
import System.Environment
import Text.Megaparsec
import Control.Monad (void)
main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)
  case parse parseVRML "" f of
    Right v -> --putDoc $ pretty v
      putStrLn ("vrml = " ++ show v)
    Left _ -> void $ parseTest parseVRML f
  --putDoc $ pretty $
  --  Node (NodeTypeId "hoge") []

