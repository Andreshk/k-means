module Main (main) where

import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import System.IO (writeFile)
import Data.List (foldl',minimumBy)
import Data.Ord (comparing)

type Point = (Double,Double)
euclDist :: Point -> Point -> Double
euclDist (a,b) (c,d) = (a-c)^2 + (b-d)^2

infixl 6 +. -- syntactic sugar, used only once :(
(+.) :: Point -> Point -> Point
(a,b) +. (c,d) = (a+c, b+d)

data Cluster = Cluster [Point] Point Int
instance Show Cluster where
    show cl@(Cluster pts _ size) = "Cluster: " ++ show pts ++
                        " {"++ show x ++ "," ++ show y ++ "} |" ++ show size ++ "|"
      where (x,y) = clusterMean cl

emptyCluster :: Cluster
emptyCluster = Cluster [] (0,0) 0

insertPoint :: Point -> Cluster -> Cluster
insertPoint p (Cluster pts sum size) = Cluster (p:pts) (p+.sum) (size+1)

clusterMean :: Cluster -> Point
clusterMean (Cluster _ (x,y) s) = let fs = fromIntegral s in (x/fs, y/fs)

makeCluster :: [Point] -> Cluster
makeCluster = foldl' (flip insertPoint) emptyCluster -- по-бързо от foldr

indexPoints :: Int -> [Point] -> [(Point,Int)]
indexPoints i pts = [ (p,i) | p<-pts ]

--- !!! heavy lifting goes here
clusterize :: (Point -> Point -> Double) -> Int -> [Point] -> [Cluster]
clusterize _ _ pts = [makeCluster pts] -- lol

nextIteration :: (Point -> Point -> Double) -> Int -> [(Point,Int)] -> [[Point]] -> [[Point]]
nextIteration dist k centers clusters = replicate k []
  where indexedPts :: [(Point,Int)]
        indexedPts = concat $ zipWith indexPoints [1..] clusters
        nearestCenter :: (Point -> Point -> Double) -> Point -> [(Point,Int)] -> Int
        nearestCenter dist p = snd . minimumBy (comparing (dist p . fst))
---- !!!

printPoint :: Point -> String
printPoint (x,y) = show x ++ " " ++ show y

readPoint :: String -> Point
readPoint str = (read x, read y)
  where (x:y:_) = words str

printSingleCluster :: Cluster -> String
printSingleCluster (Cluster pts _ _) = unlines $ map printPoint pts

printClusters :: [Cluster] -> String
printClusters clusters = concat $ zipWith (\ cl idx  -> "Cluster " ++ show idx ++ ":\n"
                                            ++ (printSingleCluster cl) ++ "\n\n") clusters [1..]

-- impure Haskell = ugly Haskell
-- ugly Haskell > ugly C++
main = do
    args <- getArgs
    if length args < 3 then
        putStrLn "Inappropriate number of arguments!"
    else do
        let (inFilename:k:outFilename:_) = args
        putStr $ "Clustering into " ++ k ++ " clusters... "
        contents <- readFile inFilename
        let points = map readPoint $ lines contents
        start <- getCPUTime
        let clusters = clusterize euclDist (read k) $! points
        end <- getCPUTime
        writeFile outFilename $ printClusters clusters
        let diff = ((fromIntegral (end - start)) / (10^12)) :: Double
        putStrLn $ "done. (" ++ show ((fromIntegral (end - start)) / (10^12)) ++ "sec)"
