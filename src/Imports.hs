module Imports (module X, module Imports) where

import Control.Applicative as X ((<$>),(<*>))
import System.IO as X
import Control.Monad as X
import Data.List as X (minimum, sort)
import Control.Lens as X
import Control.Lens.TH as X

readI :: String -> Int
readI = read

fl f [x,y] = f x y
inL e l = any (==e) l
for = flip map