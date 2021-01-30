module Proton.Par where

import Proton.Types
import Data.Profunctor.Joinable
import Control.Concurrent.Async
import Data.Profunctor

-- parrallelising :: Optic p s t a (m b) ??
parrallelising :: Joinable p Concurrently => p a (IO b) -> p a b
parrallelising  = join' . rmap Concurrently
