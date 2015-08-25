{-# LANGUAGE TupleSections #-}
module Types where
import Imports

import AI.GeneticAlgorithm.Simple
import System.Random
import Text.Printf
-- import Data.List as L
import Control.DeepSeq

import qualified Data.Map as M
import Data.Map (Map)

type Pos = (Int,Int)
type Indispo = (Int,Int)
toPos [x,y] = (x,y)

type ID = Int
data Server = Server
  { _uid :: ID
  , _width :: Int
  , _capacity :: Int
  } deriving (Show, Eq)
makeLenses ''Server

type Y = Int
type X = Int


type Group = Int
data DataCenter = DC
    { _indispo :: [Indispo]
    , _nbGroup :: Int
    , _nbRows :: Int
    , _rowLength :: Int
    , _remainingServer :: [Server]
    , _rows :: Map Y [(Server,Group,X)]
    } deriving (Show, Eq)
makeLenses ''DataCenter

instance Chromosome DataCenter where
    -- crossover :: RandomGen g => g -> a -> a -> ([a], g)
    -- mutation  :: RandomGen g => g -> a -> (a, g)
    -- fitness   :: a -> Double
    crossover g p1 p2 = ([p1,p2], g)
    mutation g p1 = (p1, g)
    fitness sol =
        let p = M.assocs (sol^.rows)
            pp = concatMap (\(x, servs) -> map (x,) servs) p
            ppp = M.fromListWith (++) $ map (\(y,(ser,grp,x)) -> (grp,[(y,ser^.capacity)])) pp
        in  myMinimum $ map (\(k,v) -> capacityPerGroup v) $ M.assocs ppp

capacityPerGroup :: [(Y, Int)] -> Double
capacityPerGroup  trucs = fromIntegral $ myMinimum $ map (\(k,v)-> sum v) $ M.assocs $ M.fromListWith (++) (map (\(k,v)-> (k, [v])) trucs)

myMinimum a = if a == [] then 0 else minimum a

instance NFData DataCenter where
    rnf a = a `seq` ()
