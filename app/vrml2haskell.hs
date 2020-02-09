{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.VRML
import System.Environment
import Text.Pretty.Simple
import Text.Pretty.Simple.Internal.OutputPrinter
import Text.Megaparsec
import Control.Monad (void)
  
myPrint =
  pPrintOpt
  NoCheckColorTty
  ( OutputOptions
    { outputOptionsIndentAmount = 2
    , outputOptionsColorOptions = Nothing
    , outputOptionsEscapeNonPrintable = True
    }
  )

main :: IO ()
main = do
  args <- getArgs
  f <- readFile (head args)
  case parse parseVRML "" f of
    Right v -> myPrint v
    Left _ -> void $ parseTest parseVRML f
