{-|
Module      : stats
Description : print statistics about the co-clusters set
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Version     : 0.1.0.0

Print statistics about the co-clusters set: number of co-clusters found, average number of rows and columns (with standard deviation), coverage and purity (for labeled examples).
-}

module Main where

import System.Environment
import Data.List (genericLength, maximum)
import Data.List.Split
import Text.Format
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> M.HashMap String (S.HashSet String)
parseFile file = M.fromList $ map parseLine (lines file )
  where
    parseLine line = let wl = words line in (head wl, S.fromList $ tail wl)

-- |'parseBic' parses the bicluster file to a list of tuples
-- representing the rows and columns of a bicluster, respectively
parseBic :: String -> [([String], [String])]
parseBic file = map parseLine $ lines file
  where
    parseLine line = let (o,f) = span (/=',') line 
                     in  (words o, words $ tail f)

-- |'mean' calculates the mean of a list of numbers
mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / (genericLength xs)

-- |'std' calculates the std of a list of numbers
std xs = sqrt $ realToFrac sumSqr / (genericLength xs)
  where
    sumSqr = sum $ map (\x -> (x - mean')^2) xs
    mean'  = mean xs

-- |'coverage' computes the coverage for the objects or features of 
-- the data set.
coverage xs = fromIntegral $ S.size $ S.unions $ map S.fromList xs

-- |'purity' calculates the purity of a cluster.
calcPurity xs = maximum counts / sum counts
  where
    counts = M.elems $ M.fromListWith (+) $ zip (map extractLabel xs) (repeat 1)
    extractLabel w = toInteger $ tail $ snd $ span (/='.') w
    toInteger x = read x :: Integer

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let
      dataName  = args !! 0

    fileIn   <- readFile $ "Datasets/" ++ dataName ++ ".data"
    fileRev  <- readFile $ "Datasets/" ++ dataName ++ "_R.data"
    fileReg  <- readFile $ "Biclusters/" ++ dataName ++ ".region.sorted"
    fileBic  <- readFile $ "Biclusters/" ++ dataName ++ ".biclusters.sorted"

    let
      dataset    = parseFile fileIn
      dataRev    = parseFile fileRev
      regions    = parseBic  fileReg
      biclusters = parseBic  fileBic

      nrows'      = map genericLength $ map fst regions
      ncols'      = map genericLength $ map snd regions

      covrows'    = coverage $ map fst regions
      perrows'    = covrows' / fromIntegral (M.size dataset)
      covcols'    = coverage $ map snd regions
      percols'    = covcols' / fromIntegral  (M.size dataRev)

      purity'     = map calcPurity $ map fst regions

      nrows      = map genericLength $ map fst biclusters
      ncols      = map genericLength $ map snd biclusters

      covrows    = coverage $ map fst biclusters
      perrows    = covrows / fromIntegral (M.size dataset)
      covcols    = coverage $ map snd biclusters
      percols    = covcols / fromIntegral  (M.size dataRev)

      purity     = map calcPurity $ map fst biclusters

    putStrLn "Regions: "
    putStrLn $ format "Number of biclusters = {0}" [show $ genericLength regions]
    putStrLn $ format "Avg. rows = {0} +/- {1}" [show $ mean nrows', show $ std nrows']
    putStrLn $ format "Avg. cols = {0} +/- {1}" [show $ mean ncols', show $ std ncols']
    putStrLn "Coverage:"
    putStrLn $ format "  rows = {0}% ({1}/{2})" [show $ 100*perrows', show $ covrows', show $ M.size dataset]
    putStrLn $ format "  cols = {0}% ({1}/{2})" [show $ 100*percols', show $ covcols', show $ M.size dataRev]
    putStrLn $ format "Purity: {0}" [show $ mean purity']

    putStrLn ""
    putStrLn "Biclusters: "
    putStrLn $ format "Number of biclusters = {0}" [show $ genericLength biclusters]
    putStrLn $ format "Avg. rows = {0} +/- {1}" [show $ mean nrows, show $ std nrows]
    putStrLn $ format "Avg. cols = {0} +/- {1}" [show $ mean ncols, show $ std ncols]
    putStrLn "Coverage:"
    putStrLn $ format "  rows = {0}% ({1}/{2})" [show $ 100*perrows, show $ covrows, show $ M.size dataset]
    putStrLn $ format "  cols = {0}% ({1}/{2})" [show $ 100*percols, show $ covcols, show $ M.size dataRev]
    putStrLn $ format "Purity: {0}" [show $ mean purity]
