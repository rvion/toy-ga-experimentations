{-# LANGUAGE TupleSections #-}
module Main where

import Imports
import Types
import qualified Data.Map as M
import AI.GeneticAlgorithm.Simple
import Data.Random.Source.StdGen (StdGen)
import System.Random(randomR)
import Debug.Trace (trace)

-- | The main entry point.
main :: IO ()
main = do
    -- Parsing
    h <- openFile "example2" ReadMode
    
    [r,s,u,p,m] <- map readI . words <$> (hGetLine h)
    indispooo <- replicateM u $ do
        toPos . map readI . words <$> (hGetLine h)
    servers <- forM [1..m] $ \x -> do
        [w,c] <- map readI . words <$> (hGetLine h)
        return $ Server {_uid=x-1, _width=w,_capacity=c}
    -- print (indispo, servers)

    -- Stating what is to solve
    let initialSol = DC
            { _indispo = indispooo
            , _nbGroup = p
            , _nbRows = r
            , _rowLength = s
            , _remainingServer = servers
            , _rows = M.fromList [(i,[])|i<-[0..r-1]]
            }
    print $ initialSol^.remainingServer^..traverse.width
    print $ initialSol^.rowLength
    print $ initialSol^.indispo
    print "plop0"
    let mkSolutionGenerator :: DataCenter -> (StdGen -> (DataCenter,StdGen))
        mkSolutionGenerator sol =
            (\g -> let (premierServer:_) = sol^.remainingServer
                       pos = myHead $ possiblePos premierServer sol yy
                       (group,g') = randomR (0,sol^.nbGroup-1) g
                       (yy,g'') = randomR (0,r-1) g'
                   in case pos of
                        Nothing -> (sol, g'')
                        Just pos -> 
                            let (sol'', gen) = (newSol group pos sol, g'')
                                next = (mkSolutionGenerator sol'') g''
                            in next
                        
            )
            where
                possiblePos :: Server -> DataCenter -> Y -> [(X,Y)]
                possiblePos serv dc ligne
                    = {- trace (show (ligne, serv)) $-} concat.concat $
                        for [0..sol^.rowLength-serv^.width] $ \i ->
                        for [0..serv^.width] $ \j -> {-trace (show (i+j, ligne)) $-}
                             let plop =  (if (not $ elem ((i+j),ligne) (_indispo dc))
                                                && (all (==True) $ map (\(s',g',x') -> not  $ elem (i+j) $ [x'..x'+s'^.width]) 
                                                                       (dc^.rows.ix ligne))
                                          then [(i+j,ligne)]
                                          else []
                                         )
                             in {-trace (show plop) -}plop
                newSol :: Int -> Pos -> DataCenter -> DataCenter
                newSol gr (x,y) sol = sol &~ do
                    let ser' = sol^.remainingServer^.to myHead
                    case ser' of 
                        Nothing -> {-trace "nooo"-} return ()
                        Just ser -> do
                            rows.ix y %= ((ser,gr,x):)
                            remainingServer %= tail
                            return ()
    print "plop"
    r <- runGAIO 10 0.1 (mkSolutionGenerator initialSol)
            (\a i -> return $ if i > 20 then True else False)
    print "plop2"
    print (r^.rows)
    -- Solving
    let sol = r
        p = M.assocs (sol^.rows)
        pp = concatMap (\(x, servs) -> map (x,) servs) p
        ppp = M.fromList $ map (\(y,(ser,grp,x)) -> (ser^.uid,(x,y,grp))) pp
    print "plop3"
    forM [0..m-1] $ \i -> do
        case M.lookup i ppp of
            Just (x,y,grp) -> print $ unwords $ map show [x, y, grp]
            Nothing -> print "x"

    return ()

myHead l= if l==[] then Nothing else Just (head l)
