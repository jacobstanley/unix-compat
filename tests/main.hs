module Main where

import MkstempSpec
import LinksSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    mkstempSpec
    linksSpec
