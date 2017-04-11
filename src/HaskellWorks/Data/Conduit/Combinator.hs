module HaskellWorks.Data.Conduit.Combinator where

import           Data.Conduit
import           Data.Conduit.List as L
import           Data.Maybe

inJust :: Monad m => Conduit a m c -> Conduit (Maybe a) m (Maybe c)
inJust c = getZipConduit
      $   ZipConduit (L.filter isNothing  .|      L.map (const Nothing))
      <*  ZipConduit (L.concat            .| c .| L.map Just           )
