{-|
Module      : candidates
Description : group lsh data into a candidate set
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

group the objects by lsh keys to become subspace regions to search for biclusters.
-}

module Main where

import System.Environment
import Data.List (genericLength, maximum)
import Data.List.Split
import Text.Format
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> M.HashMap String (S.Set String)
parseFile file = M.fromList $ map parseLine (lines file )
  where
    parseLine line = let wl = words line in (head wl, S.fromList $ tail wl)

parseBic :: String -> [([String], [String])]
parseBic file = map parseLine $ lines file
  where
    parseLine line = let (o,f) = span (/=',') line in (words o, words $ tail f)

--length' = fromIntegral . length

mean :: (Real a, Fractional b) => [a] -> b
mean l = realToFrac (sum l) / (genericLength l)

std l = sqrt $ realToFrac sumSqr / (genericLength l)
  where
    sumSqr = sum $ map (\x -> (x - mean')^2) l
    mean'  = mean l

coverage l = fromIntegral $ S.size $ S.unions $ map S.fromList l -- nub $ foldl' (++) [] l

calcPurity l = maximum counts / sum counts
  where
    counts = M.elems $ M.fromListWith (+) $ zip (map extractLabel l) (repeat 1)
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

    print "Regions: "
    print $ format "Number of biclusters = {0}" [show $ genericLength regions]
    print $ format "Avg. rows = {0} +/- {1}" [show $ mean nrows', show $ std nrows']
    print $ format "Avg. cols = {0} +/- {1}" [show $ mean ncols', show $ std ncols']
    print "Coverage:"
    print $ format "  rows = {0}% ({1}/{2})" [show $ 100*perrows', show $ covrows', show $ M.size dataset]
    print $ format "  cols = {0}% ({1}/{2})" [show $ 100*percols', show $ covcols', show $ M.size dataRev]
    print $ format "Purity: {0}" [show $ mean purity']

    print "\nBiclusters: "
    print $ format "Number of biclusters = {0}" [show $ genericLength biclusters]
    print $ format "Avg. rows = {0} +/- {1}" [show $ mean nrows, show $ std nrows]
    print $ format "Avg. cols = {0} +/- {1}" [show $ mean ncols, show $ std ncols]
    print "Coverage:"
    print $ format "  rows = {0}% ({1}/{2})" [show $ 100*perrows, show $ covrows, show $ M.size dataset]
    print $ format "  cols = {0}% ({1}/{2})" [show $ 100*percols, show $ covcols, show $ M.size dataRev]
    print $ format "Purity: {0}" [show $ mean purity]
