{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as Text
import Plutarch.Prelude

hello :: ClosedTerm PString
hello = "Hello from Plutarch"

main :: IO ()
main = Text.putStrLn $ plift hello
