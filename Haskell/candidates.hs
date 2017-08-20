{-|
Module      : candidates
Description : group lsh data into a candidate set
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

group the objects by lsh keys to become subspace regions to search for biclusters.
-}

module Main where

import Streaming
import qualified Streaming.Prelude as S
import System.Environment
import Data.List (intercalate)
import Data.Hashable (hash)
import Text.Format

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> (String, String)
parseFile line = (head wl, last wl)
  where
    wl = words line

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let fileIn  = args !! 0
    let fileOut = args !! 1
    let nrows   = read (args !! 2) :: Int

    runResourceT $ S.writeFile fileOut
                 $ S.map (intercalate " ")
                 $ S.map (map snd)
                 $ S.filter (\xs -> length xs > nrows) 
                 $ mapped S.toList 
                 $ S.groupBy (\x y -> fst x == fst y) 
                 $ S.map parseFile $ S.readFile fileIn
