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
import Text.Printf
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as I


-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: T.Text -> M.HashMap T.Text (S.HashSet T.Text)
parseFile file = M.fromList $ map parseLine (T.lines file )
  where
    parseLine line = let wl = T.words line in (head wl, S.fromList $ tail wl)

-- |'parseBic' parses the bicluster file to a list of tuples
-- representing the rows and columns of a bicluster, respectively
parseBic :: T.Text -> [([T.Text], [T.Text])]
parseBic file = map parseLine $ T.lines file
  where
    parseLine line = let (o,f) = T.span (/=',') line 
                     in  (T.words o, T.words $ T.tail f)

-- |'mean' calculates the mean of a list of numbers
mean :: (Real a) => [a] -> Float
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
    extractLabel w = T.tail $ snd $ T.span (/='.') w

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let
      dataName  = args !! 0

    fileIn   <- I.readFile $ "Datasets/" ++ dataName ++ ".data"
    fileRev  <- I.readFile $ "Datasets/" ++ dataName ++ "_R.data"
    fileReg  <- I.readFile $ "Biclusters/" ++ dataName ++ ".region.sorted"
    fileBic  <- I.readFile $ "Biclusters/" ++ dataName ++ ".biclusters.sorted"

    let
      dataset    = parseFile fileIn
      dataRev    = parseFile fileRev
      regions    = parseBic  fileReg
      biclusters = parseBic  fileBic

      nrows'      = map genericLength $ map fst regions
      ncols'      = map genericLength $ map snd regions

      meanrows'   = mean nrows'
      stdrows'    = std nrows' :: Double

      covrows'    = coverage $ map fst regions
      perrows'    = covrows' / fromIntegral (M.size dataset) :: Double
      covcols'    = coverage $ map snd regions
      percols'    = covcols' / fromIntegral  (M.size dataRev) :: Double

      purity'     = map calcPurity $ map fst regions

      nrows      = map genericLength $ map fst biclusters
      ncols      = map genericLength $ map snd biclusters

      covrows    = coverage $ map fst biclusters
      perrows    = covrows / fromIntegral (M.size dataset) :: Double
      covcols    = coverage $ map snd biclusters
      percols    = covcols / fromIntegral  (M.size dataRev) :: Double

      purity     = map calcPurity $ map fst biclusters

    putStrLn "Regions: "
    putStrLn $ printf "Number of biclusters = %d" (genericLength regions :: Integer)
    putStrLn $ printf "Avg. rows = %.2f +/- %.2f" meanrows' stdrows'
    putStrLn $ printf "Avg. cols = %.2f +/- %.2f" (mean ncols') (std ncols' :: Double)
    putStrLn "Coverage:"
    putStrLn $ printf "  rows = %.2f%% (%f/%d)" (100.0*perrows') (covrows') (M.size dataset)
    putStrLn $ printf "  cols = %.2f%% (%f/%d)" (100.0*percols') (covcols') (M.size dataRev)
    putStrLn $ printf "Purity: %.4f" (mean purity')
    putStrLn ""
    putStrLn "Biclusters: "
    putStrLn $ printf "Number of biclusters = %d" (genericLength biclusters :: Integer)
    putStrLn $ printf "Avg. rows = %.2f +/- %.2f" (mean nrows) (std nrows :: Double)
    putStrLn $ printf "Avg. cols = %.2f +/- %.2f" (mean ncols) (std ncols :: Double)
    putStrLn "Coverage:"
    putStrLn $ printf "  rows = %.2f%% (%f/%d)" (100.0*perrows) (covrows) (M.size dataset)
    putStrLn $ printf "  cols = %.2f%% (%f/%d)" (100.0*percols) (covcols) (M.size dataRev)
    putStrLn $ printf "Purity: %.4f" (mean purity)
