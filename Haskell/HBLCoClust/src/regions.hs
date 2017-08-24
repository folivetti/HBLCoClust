{-|
Module      : regions
Description : Find the promising regions of co-clusters 
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Version     : 0.1.0.0

Group the lsh buckets and filter those that defines promising regions containing co-clusters.
-}

module Main where

import System.Environment
import Data.List (intercalate, nub, sort, groupBy)
import Text.Format
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type Dataset = M.HashMap String (S.HashSet String)

-- | support functions and operators

toString (o,f) = (intercalate " " o) ++ "," ++ (intercalate " " f)
toStringFeat f = intercalate " " f

sortedUnique xs = map head $ groupBy (==) $ sort xs

(∩) s1 s2 =  S.intersection s1 s2

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> Dataset
parseFile file = M.fromList $ map parseLine (lines file )
  where
    parseLine line = let wl = words line in (head wl, S.fromList $ tail wl)

-- |'parseCand' parses the .candidates file
parseCand :: String -> [[String]]
parseCand file = map words $ lines file

-- |'findRegions' returns the region defined by a list of objects.
--  The region is formed by the features common to every obj and
-- the objects common to every feature of feats'.
findRegions :: Dataset -> Dataset -> [[String]] -> [([String],[String])]
findRegions dataset dataRev candidates = map getIntersection candidates
  where
    getIntersection objs = 
      let
        objs'  = if length feats' > 0 
                 then S.toList $ foldl1 (∩) $ map (dataRev M.!) feats' 
                 else []
        feats' = S.toList $ foldl1 (∩) $ map (dataset M.!) objs
      in (objs', feats')

-- |'expand' expands the region further by allowing features with at least 
-- nrows objects from the list. Returns the subsets of features to enumreate.
expand :: Dataset -> Dataset -> Int -> Int -> [([String],[String])] -> [[String]]
expand dataset dataRev nrows ncols regions = map expandRegion regions
  where
    expandRegion (obj, feat) = 
      let
        feat'        = filter (\f -> commonObjs f >= nrows) $ M.keys dataRev
        commonObjs f = S.size $ (objsOf f) ∩ objSet
        objsOf f     = dataRev M.! f
        objSet       = S.fromList obj
      in sortedUnique $ feat ++ feat'

-- |'reduce' removes the repeated regions found after the expansion
reduce :: [[String]] -> [[String]]
reduce regions = map S.toList unique
  where
    unique = map S.unions grouped
    grouped = groupBy anyIntersection setRegions
    setRegions = map S.fromList regions
    anyIntersection s1 s2 = let inter = s1 ∩ s2 
                            in (inter == s1) || (inter == s2)

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let
      dataName  = args !! 0
      nrows   = read (args !! 1) :: Int
      ncols   = read (args !! 2) :: Int

    fileIn   <- readFile $ "Datasets/" ++ dataName ++ ".data"
    fileRev  <- readFile $ "Datasets/" ++ dataName ++ "_R.data"
    fileCand <- readFile $ "Candidates/" ++ dataName ++ ".cand.sorted"

    let
      dataset    = parseFile fileIn
      dataRev    = parseFile fileRev
      candidates = parseCand fileCand
      minVolume (r,c) = length r >= nrows && length c >= ncols
      regions    = filter minVolume $ findRegions dataset dataRev candidates
      expanded   = reduce $ sort $ expand dataset dataRev nrows ncols regions
      fileOut1    = "Biclusters/" ++ dataName ++ ".region"
      fileOut2    = "Biclusters/" ++ dataName ++ ".expanded"
    writeFile fileOut1 (unlines $ map toString regions)
    writeFile fileOut2 (unlines $ map toStringFeat expanded)
