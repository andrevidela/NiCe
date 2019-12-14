{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Test.Hspec
import ParseSpec
import Protolude

main :: IO ()
main = hspec letSpec
