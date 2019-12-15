{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Test.Hspec
import ParseSpec
import LexerSpec
import Protolude

main :: IO ()
main = do hspec lexSpec
          hspec letSpec
