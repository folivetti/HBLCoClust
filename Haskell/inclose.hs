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
import Data.List (intercalate, any)
import Text.Format
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> M.HashMap String (S.Set String)
parseFile file = M.fromList $ map parseLine (lines file )
  where
    parseLine line = let wl = words line in (head wl, S.fromList $ tail wl)

parseRegion :: String -> [[String]]
parseRegion file = map parseLine $ lines file
  where
    parseLine line = words line --let (o,f) = span (/=',') line in (words o, words $ tail f)

(∩) s1 s2 =  S.intersection s1 s2

inClose dataset dataRev nrows ncols feats = inClose' nextF (intent nextF, [nextF])
  where
    nextF = head feats
    intent fi = dataRev M.! fi
    inClose' fi (o, f) = bic ++ concat [inClose' fi'' (o'', f'') | (fi'', o'', f'') <- candidates]
      where
        bic         = if length f' >= ncols then [(S.toList o, f')] else []
        f'          = f ++ [f'' | (f'', o'') <- inserts, o'' == o]
        inserts     = filterByRow [(f'', o ∩ (intent f'')) | f'' <- nextFeats]

        candidates   = [(fi', o'', f ++ [fi']) | (fi',o'') <- inserts, o'' /= o, cannonical (o'', f ++ [fi']) fi']
        cannonical (o'', f'') fi' = any (==o'') [o'' ∩ (intent fi'') | fi'' <- fst $ span (/=fi') feats, not (elem fi'' f'')]

        nextFeats   = tail $ snd $ span (/=fi) feats
        filterByRow = filter (\(f'',o'') -> S.size o''>= nrows)

toString (o,f) = (intercalate " " o) ++ "," ++ (intercalate " " f)

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

