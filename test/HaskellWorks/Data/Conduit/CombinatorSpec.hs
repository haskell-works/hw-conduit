{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module HaskellWorks.Data.Conduit.CombinatorSpec (spec) where

import Data.Conduit
import Data.Conduit.List                    as L
import Data.Either                          (lefts, rights)
import Data.Functor.Identity
import HaskellWorks.Data.Conduit.Combinator

import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

maybes :: [Maybe Int]
maybes = [Just 1, Nothing, Just 2, Nothing]

eithers :: [Either String Int]
eithers = [Left "err-1", Right 1, Left "err-2", Right 2]

spec :: Spec
spec = describe "HaskellWorks.Data.Conduit.CombinatorSpec" $ do
  describe "inJust" $ do
    it "should map when Just and propagate Nothing" $ do
      let input = [Just 1, Just 2, Nothing, Nothing, Just 4, Nothing] :: [Maybe Int]
      let results = runConduitPure $
                L.sourceList input
            .|  inJust (L.map (+1))
            .|  L.consume
      results `shouldBe` [Just 2, Just 3, Nothing, Nothing, Just 5, Nothing]

    it "should project Nothing" $ do
      runList maybes projectNothings `shouldBe` [(), ()]

    it "should project Left" $ do
      runList eithers projectLefts `shouldBe` lefts eithers

    it "should project Right" $ do
      runList eithers projectRights `shouldBe` rights eithers

-------------------------------------------------------------------------------
runList :: [a] -> Conduit a Identity b -> [b]
runList src f = runConduitPure (L.sourceList src .| f .| L.consume)
