{-|
Module      : inclose
Description : InClose-2 algorithm for Formal Concepts Analysis
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Version     : 0.1.0.0

Enummerative algorithm for Formal Concepts Analysis. Equivalent to find every dense co-cluster within a region for binary data sets.
-}

module Main where

import System.Environment
import Data.List (intercalate, all)
import Text.Format
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

-- | support functions and operators

(∩) s1 s2 =  S.intersection s1 s2

toString (os, fs) = (intercalate " " os) ++ "," ++ (intercalate " " fs)

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> M.HashMap String (S.HashSet String)
parseFile file = M.fromList $ map parseLine (lines file )
  where
    parseLine line = let wl = words line in (head wl, S.fromList $ tail wl)

-- |'parseRegion' parses the .region file
-- containing a list of features to explore
parseRegion :: String -> [[String]]
parseRegion file = map parseLine $ lines file
  where
    parseLine line = words line

-- |'inClose' algorithm
inClose dataset dataRev nrows ncols feats = inClose' (firstFeat, extent firstFeat, [firstFeat])
  where
    firstFeat = head feats
    extent fi = dataRev M.! fi

    cannonical (fi', o'', f'') = all (/=o'') [o'' ∩ (extent fi'') | fi'' <- fst $ span (/=fi') feats, not (elem fi'' f'')]

    -- |'inClose'' is the recursive algorithm
    -- fi is the last feature inserted into the co-cluster
    -- defined by a list 'os' of objects and 'fs' of features
    -- *i one element, *s a list
    inClose' (fi, os, fs)
      | length closedFeats >= ncols = coCluster : (concat $ map inClose' candidates)
      | otherwise                   = concat $ map inClose' candidates
      where
        coCluster   = (S.toList os, closedFeats)
        closedFeats = fs ++ map fst canClose 

        -- generates (fi', os')
        inserts     = filterByRow [(fi', os ∩ (extent fi')) | fi' <- nextFeats]
        canClose    = filter ((==os) . snd) inserts
        cannotClose = filter ((/=os) . snd) inserts

        -- generates (fi', os', fs')
        candidates  = filter cannonical $ map genCandidate cannotClose
        nextFeats   = tail $ snd $ span (/=fi) feats
        filterByRow = filter (\(f'',o'') -> S.size o''>= nrows)
        genCandidate (fi', os') = (fi', os', fs ++ [fi'])

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let
      dataName = args !! 0
      nrows    = read (args !! 1) :: Int
      ncols    = read (args !! 2) :: Int

    fileIn     <- readFile $ "Datasets/" ++ dataName ++ ".data"
    fileRev    <- readFile $ "Datasets/" ++ dataName ++ "_R.data"
    fileRegion <- readFile $ "Biclusters/" ++ dataName ++ ".expanded.sorted"

    let
      dataset    = parseFile fileIn
      dataRev    = parseFile fileRev
      regions    = parseRegion fileRegion
      biclusters = concat $ map (inClose dataset dataRev nrows ncols) regions
      fileOut    = "Biclusters/" ++ dataName ++ ".biclusters"

    writeFile fileOut (unlines $ map toString biclusters)

