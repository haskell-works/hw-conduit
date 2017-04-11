module HaskellWorks.Data.Conduit.CombinatorSpec (spec) where

import           Data.Conduit
import           Data.Conduit.List                    as L
import           Data.Maybe
import           HaskellWorks.Data.Conduit.Combinator

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

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
