
module HaskellWorks.Data.Conduit.List
  ( runListConduit
  ) where

import Data.Conduit
import Prelude

import qualified Data.Conduit.List as CL
import qualified Prelude           as P

runListConduit :: ConduitT i o [] () -> [i] -> [o]
runListConduit c is = P.concat $ runConduit $ CL.sourceList is .| c .| CL.consume
