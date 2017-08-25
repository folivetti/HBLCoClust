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

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

-- | support functions and operators

(∩) s1 s2 =  S.intersection s1 s2

sepSpace = C.pack " "
sepComma = C.pack ","

toString (os,fs) = B.intercalate sepComma [B.intercalate sepSpace os, B.intercalate sepSpace fs]

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: B.ByteString -> M.HashMap B.ByteString (S.HashSet B.ByteString)
parseFile file = M.fromList $ map parseLine (C.lines file )
  where
    parseLine line = let wl = C.words line in (head wl, S.fromList $ tail wl)

-- |'parseRegion' parses the .region file
-- containing a list of features to explore
parseRegion :: B.ByteString -> [[B.ByteString]]
parseRegion file = map C.words $ C.lines file

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

    fileIn     <- B.readFile $ "Datasets/" ++ dataName ++ ".data"
    fileRev    <- B.readFile $ "Datasets/" ++ dataName ++ "_R.data"
    fileRegion <- B.readFile $ "Biclusters/" ++ dataName ++ ".expanded.sorted"

    let
      dataset    = parseFile fileIn
      dataRev    = parseFile fileRev
      regions    = parseRegion fileRegion
      biclusters = concat $ map (inClose dataset dataRev nrows ncols) regions
      fileOut    = "Biclusters/" ++ dataName ++ ".biclusters"

    B.writeFile fileOut (C.unlines $ map toString biclusters)

