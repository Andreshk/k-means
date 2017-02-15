module Main (main) where

import System.Environment (getArgs)
import System.Clock (TimeSpec, Clock(Realtime), getTime, diffTimeSpec, toNanoSecs) -- requires "clock" package
import System.IO (readFile,writeFile,hFlush,stdout)
import Data.List (foldl',minimumBy)
import Data.Ord (comparing)

type Point = (Double,Double)
dist :: Point -> Point -> Double
dist (a,b) (c,d) = (a-c)^2 + (b-d)^2

-- Main heavy-lifting function, generating the initial centers and
-- clusters and passing them to the looping nextIteration function.
-- Returns the final clusters as well as the number of iterations needed.
-- Each point is given an index in the range [1..k], showing to which cluster it belongs.
clusterize :: Int -> [Point] -> ([[Point]], Int)
clusterize k points = nextIteration k initialCenters initialClusters 1
  where initialCenters = zip (take k points) [1..] --lol, needs changing
        initialClusters = fst $ reClusterize initialCenters [(p,-1) | p<-points]

-- At every step this function calculates the new cluster centers,
-- reclusterizes the points and breaks when no point has changed its cluster.
nextIteration :: Int -> [(Point,Int)] -> [(Point,Int)] -> Int -> ([[Point]], Int)
nextIteration k centers points iter
  | isStable  = (groupToClusters k newpoints, iter)
  | otherwise = nextIteration k newcenters newpoints (iter+1)
  where newcenters = calculateMeans k points
        (newpoints,isStable) = reClusterize newcenters points

-- Groups the indexed points in simple lists by their cluster index.
groupToClusters :: Int -> [(Point,Int)] -> [[Point]]
groupToClusters k points = [ [ p | (p,j)<-points, i==j ] | i<-[1..k]]

-- Calculates all clusters' means by firstly grouping the indexed points by clusters.
calculateMeans :: Int -> [(Point,Int)] -> [(Point,Int)]
calculateMeans k points = zip (map mean $ groupToClusters k points) [1..]
  where mean pts = let (xs,ys) = unzip pts
                       len = fromIntegral $ length pts
                   in (sum xs / len, sum ys / len)

-- Recalculates the nearest center for every point, while keeping
-- check whether a single point has changed its cluster. Done with
-- a simple strict fold of the indexed point list.
reClusterize :: [(Point,Int)] -> [(Point,Int)] -> ([(Point,Int)], Bool)
reClusterize centers = foldl' (\(result,flag) (p,i) -> let newI = snd $ minimumBy (comparing (dist p . fst)) centers
                                                       in ((p,newI):result, flag && newI == i)) ([],True)

-- Small, useful IO-related functions
readPoint :: String -> Point
readPoint str = (read x, read y)
  where (x:y:_) = words str

printClusters :: [[Point]] -> String
printClusters clusters = concat $ zipWith (\ cl idx  -> "Cluster " ++ show idx ++ ":\n" ++ (printCl cl) ++ "\n") clusters [1..]
  where printCl = unlines . map (\(x,y) -> show x ++ " " ++ show y)

-- impure Haskell = ugly Haskell
-- ugly Haskell > ugly C++
main = do
    args <- getArgs
    if length args < 3 then
        putStrLn "Inappropriate number of arguments!"
    else do
        let (inFilename:k:outFilename:_) = args
        putStr $ "Clustering into " ++ k ++ " clusters... "
        hFlush stdout
        start <- getTime Realtime
        contents <- readFile inFilename
        let points = map readPoint $ lines contents
            (clusters, iterations) = clusterize (read k) points
        writeFile outFilename $ printClusters clusters
        end <- getTime Realtime
        putStrLn $ "done. (" ++ show iterations ++ " iterations, " ++ show (milliseconds start end) ++ "msec)"
  where milliseconds start end = fromIntegral (toNanoSecs (diffTimeSpec start end)) / 10^6

-- iei
