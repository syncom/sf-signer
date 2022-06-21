{-# LANGUAGE OverloadedStrings #-}

module Main where

import SignerLib
import CliLib
import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text as T

main :: IO ()
main = do
  cmd <- options "A file signing utility with X.509 certificates" parser
  cmd
