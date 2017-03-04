{-# LANGUAGE FlexibleInstances #-}
module Main (main) where

import System.Environment (getArgs, withArgs)
import System.Random (mkStdGen, randomRs)
import System.Clock (TimeSpec, Clock(Realtime), getTime, diffTimeSpec, toNanoSecs) -- requires "clock" package
import System.IO (readFile, writeFile, hFlush, stdout)
import Data.List (foldl', minimumBy, nub)
import Data.Ord (comparing)

class Clusterizable a where
  dist      :: a -> a -> Double
  mean      :: [a] -> a
  fromFile  :: String -> a
  toFile    :: a -> String

type Point = (Double,Double)
instance Clusterizable Point where
  dist (a,b) (c,d)  = (a-c)^2 + (b-d)^2
  mean pts          = let (xs,ys,len) = foldl' (\(ax,ay,len) (x,y) -> (ax+x,ay+y,len+1)) (0,0,0) pts in (xs / len, ys / len)
  fromFile str      = let (x:y:_) = words str in (read x, read y)
  toFile (x,y)      = show x ++ " " ++ show y

-- Strict and fused map + length (+ reverse)
mapAndLength :: (a -> b) -> [a] -> ([b],Int)
mapAndLength f = foldl' (\(result,len) x -> ((f x):result, len+1)) ([],0)

-- Main heavy-lifting function, generating the initial centers and
-- clusters and passing them to the looping nextIteration function.
-- Receives the randomly generated indices of the initial cluster centers.
-- Returns the final clusters as well as the number of iterations needed.
-- Each point is given an index in the range [1..k], showing to which cluster it belongs.
clusterize :: Clusterizable a => Int -> [Int] -> [a] -> ([[a]], Int)
clusterize k centerIdxs points = nextIteration k initialCenters initialClusters 1
  where initialCenters = zip (map (points!!) centerIdxs) [1..]
        initialClusters = fst $ reClusterize initialCenters [(p,-1) | p<-points]

-- At every step this function calculates the new cluster centers,
-- reclusterizes the points and breaks when no point has changed its cluster.
nextIteration :: Clusterizable a => Int -> [(a,Int)] -> [(a,Int)] -> Int -> ([[a]], Int)
nextIteration k centers points iter
  | isStable  = (groupToClusters k newpoints, iter)
  | otherwise = nextIteration k newcenters newpoints (iter+1)
  where newcenters = calculatemeans k points
        (newpoints,isStable) = reClusterize newcenters points

-- Groups the indexed points in simple lists by their cluster index.
groupToClusters :: Int -> [(a,Int)] -> [[a]]
groupToClusters k points = [ [ p | (p,j)<-points, i==j ] | i<-[1..k]]

-- Calculates all clusters' means by firstly grouping the indexed points by clusters.
calculatemeans :: Clusterizable a => Int -> [(a,Int)] -> [(a,Int)]
calculatemeans k points = zip (map mean $ groupToClusters k points) [1..]

-- Recalculates the nearest center for every point, while keeping
-- check whether any single point has changed its cluster. Done with
-- a simple strict fold of the indexed point list.
reClusterize :: Clusterizable a => [(a,Int)] -> [(a,Int)] -> ([(a,Int)], Bool)
reClusterize centers = foldl' (\(result,flag) (p,i) -> let newI = snd $ minimumBy (comparing (dist p . fst)) centers
                                                       in ((p,newI):result, flag && newI == i)) ([],True)

-- Quantify the minimum reached with the clusterization. The algorithm basically
-- tries to assign each point to a cluster, in a way that minimizes this amount.
-- It is being printed on the standard output in order for the user to judge whether
-- a local or a global minimum has been reached. WCSS stands for within-cluster sum of squares.
wcss :: Clusterizable a => [[a]] -> Double
wcss clusters = sum [ sum [ dist center p | let center = mean cl, p<-cl ] | cl<-clusters]

-- Small, useful IO-related function
printClusters :: Clusterizable a => [[a]] -> String
printClusters clusters = concat $ zipWith (\ cl idx  -> "Cluster " ++ show idx ++ ":\n" ++ (printCl cl) ++ "\n") clusters [1..]
  where printCl = unlines . map toFile

-- impure Haskell = ugly Haskell
-- ugly Haskell > ugly C++
main = do
    args <- getArgs
    if length args < 3 then
        putStrLn "Inappropriate number of arguments!"
    else do
        let (inFilename:k':outFilename:_) = args
            k = read k'
        putStr $ "Clustering into " ++ k' ++ " clusters... "
        hFlush stdout
        start <- getTime Realtime
        contents <- readFile inFilename
        let (points, n) = mapAndLength fromFile $ lines contents :: ([Point],Int)
        if n < k then
            putStrLn "error: #clusters should be < #datapoints!"
        else do
          let centerIdxs = take k . nub . randomRs (0, n-1) . mkStdGen . fromIntegral . toNanoSecs $ start
              (clusters, iterations) = clusterize k centerIdxs points
              minReached = wcss clusters
          writeFile outFilename $ printClusters clusters
          end <- getTime Realtime
          putStrLn $ "done. (" ++ show iterations ++ " iterations, "
                               ++ show (milliseconds start end) ++ "msec, wcss="
                               ++ show minReached ++ ")"
  where milliseconds start end = fromIntegral (toNanoSecs (diffTimeSpec start end)) / 10^6

-- User-friendly version - to be called from GHCI or WinGHCI
kmeans :: String -> Int -> String -> IO ()
kmeans inFilename k outFilename = withArgs [inFilename, (show k), outFilename] main

-- iei
