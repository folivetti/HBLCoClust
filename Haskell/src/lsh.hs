{-|
Module      : lsh
Description : applies lsh to a binary data set
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Version     : 0.1.0.0

applies LSH to categorical data for the first step of the HBLCoClust algorithm. Approximates Jaccard similarity.
-}

module Main where

import System.Random
import Streaming
import qualified Streaming.Prelude as S
import System.Environment
import qualified Data.List.Split as Sp (chunksOf)

import Data.List (minimum, intercalate, sort)
import Data.Hashable (hash)
import Text.Format

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> (String, [String])
parseFile line = (head wl, tail wl)
  where
    wl = words line

largeprime = 109297

hash' :: (Int, Int) -> Int -> Int
hash' (n1, n2) h = (n1*h + n2) `mod` largeprime

-- |'lsh' generates 'r' LSH signatures with 'b' hashes each
lsh :: Int -> Int -> [(Int,Int)] -> (String, [String]) -> [String]
lsh r b rnd (label, ws) = map output lshSigns
  where
    output signature = format "{0} {1}" [signature, label]
    lshSigns         = sort $ map (intercalate "_") lshList
    lshList          = map addIdx $ zip (Sp.chunksOf b minhash) [1..]
    addIdx (xs, idx) = show idx : xs
    minhash          = map genSignature rnd
    genSignature n   = show $ minimum $ map (hash' n) hashedWords
    hashedWords      = map hash ws

-- |'main' runs the main program
main :: IO ()
main = do
    args <- getArgs
    g <- getStdGen

    let 
      dataName = args !! 0        -- dataset name
      r = read (args !! 1) :: Int -- number of bands
      b = read (args !! 2) :: Int -- hashes per bands

      fileIn  = "Datasets/" ++ dataName ++ ".data"
      fileOut = "LSH/" ++ dataName ++ ".lsh"

      n1 = take (r*b) (randomRs (1, largeprime-1) g)
      n2 = take (r*b) (randomRs (1, largeprime-1) g)
      rnd = zip n1 n2
      lsh' = lsh r b rnd

    -- stream the input into parseFile -> lsh' and into the output
    runResourceT $ S.writeFile fileOut $ S.map unlines $ S.map lsh' $ S.map parseFile $ S.readFile fileIn
