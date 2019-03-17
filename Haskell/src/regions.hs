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
import Data.List (intercalate, nub, sort, sortBy, groupBy, reverse)
import Data.Ord
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as I

type StringSet = S.Set T.Text
type Dataset   = M.HashMap T.Text StringSet

-- | support functions and operators

debugMap :: Dataset -> T.Text -> StringSet
debugMap map' elem = do
                      let maybeV = M.lookup elem map'
                      case maybeV of 
                           Just v -> v
                           Nothing -> error $ (show elem) ++ " not found"

sepSpace = T.pack " "
sepComma = T.pack ","

toString (o,f) = T.intercalate sepComma [T.intercalate sepSpace $ S.toList o, T.intercalate sepSpace $ S.toList f]
toStringFeat f = T.intercalate sepSpace $ S.toList f

sortedUnique xs = map head $ groupBy (==) $ sort xs

(∩) s1 s2 =  S.intersection s1 s2
(∪) s1 s2 = S.union s1 s2

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: T.Text -> Dataset
parseFile file = M.fromList $ map parseLine (T.lines file )
  where
    parseLine line = let wl = T.words line in (head wl, S.fromList $ tail wl)

-- |'parseCand' parses the .candidates file
parseCand :: T.Text -> [[T.Text]]
parseCand file = map T.words $ T.lines file

-- |'findRegions' returns the region defined by a list of objects.
--  The region is formed by the features common to every obj and
-- the objects common to every feature of feats'.
findRegions :: Dataset -> Dataset -> [[T.Text]] -> [(StringSet,StringSet)]
findRegions dataset dataRev candidates = map getIntersection candidates
  where
    getIntersection objs = 
      let
        objs'  = if S.size feats' > 0 
                 then foldl1 (∩) $ map (dataRev M.!) $ S.toList feats'  -- (dataRev M.!)
                 else S.empty
        feats' = foldl1 (∩) $ map (dataset M.!) objs --map (dataset M.!) objs
      in (objs', feats')

-- |'expand' expands the region further by allowing features with at least 
-- nrows objects from the list. Returns the subsets of features to enumreate.
expand :: Dataset -> Dataset -> Int -> Int -> [(StringSet,StringSet)] -> [StringSet]
expand dataset dataRev nrows ncols regions = map expandRegion regions
  where
    expandRegion (obj, feat) = 
      let
        feat'        = S.fromList $ filter (\f -> commonObjs f >= nrows) $ M.keys dataRev
        commonObjs f = S.size $ (objsOf f) ∩ obj
        objsOf f     = dataRev M.! f
      in feat ∪ feat'

-- |'reduce' removes the repeated regions found after the expansion
reduce :: [StringSet] -> [StringSet]
reduce []     = []
reduce (x:xs) = x : reduce filtered
  where
    filtered    = filter notSubSet xs
    notSubSet y = not (S.isSubsetOf y x)

bySize s1 s2
  | S.size s1 >= S.size s2 = LT
  | otherwise = GT

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let
      dataName  = args !! 0
      nrows   = read (args !! 1) :: Int
      ncols   = read (args !! 2) :: Int

    fileIn   <- I.readFile $ "Datasets/" ++ dataName ++ ".data"
    fileRev  <- I.readFile $ "Datasets/" ++ dataName ++ "_R.data"
    fileCand <- I.readFile $ "Candidates/" ++ dataName ++ ".cand.sorted"

    let
      dataset    = parseFile fileIn
      dataRev    = parseFile fileRev
      candidates = parseCand fileCand
      minVolume (r,c) = S.size r >= nrows && S.size c >= ncols
      regions    = filter minVolume $ findRegions dataset dataRev candidates
      expanded   = reduce $ sortBy bySize $ expand dataset dataRev nrows ncols regions
      fileOut1    = "Biclusters/" ++ dataName ++ ".region"
      fileOut2    = "Biclusters/" ++ dataName ++ ".expanded"
    I.writeFile fileOut1 (T.unlines $ map toString regions)
    I.writeFile fileOut2 (T.unlines $ map toStringFeat expanded)
