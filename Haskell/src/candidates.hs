{-|
Module      : candidates
Description : group lsh data into a candidate set
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Version     : 0.1.0.0

group the objects by lsh keys to become subspace regions to search for biclusters.
-}

module Main where

import System.Environment
import Data.List (groupBy)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as I

sep = T.pack " "

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: T.Text -> (T.Text, T.Text)
parseFile line = (head wl, last wl)
  where
    wl = T.words line

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let
      dataName  = args !! 0    
      nrows   = read (args !! 1) :: Int

      fileIn  = "LSH/" ++ dataName ++ ".lsh.sorted"
      fileOut = "Candidates/" ++ dataName ++ ".cand"

    content <- I.readFile fileIn
    I.writeFile fileOut $ T.unlines
                        $ map (T.intercalate sep)
                        $ map (map snd)
                        $ filter (\xs -> length xs >= nrows)
                        $ groupBy (\x y -> fst x == fst y)
                        $ map parseFile 
                        $ T.lines content
{-
    runResourceT $ S.writeFile fileOut
                 $ S.map (B.intercalate sep)
                 $ S.map (map snd)
                 $ S.filter (\xs -> length xs >= nrows) 
                 $ mapped S.toList 
                 $ S.groupBy (\x y -> fst x == fst y) 
                 $ S.map parseFile $ S.readFile fileIn
-}
