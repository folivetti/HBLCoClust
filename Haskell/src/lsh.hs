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
import System.Environment
import qualified Data.List.Split as Sp (chunksOf)

import Data.List (minimum, intercalate, sort)
import Data.Hashable (hash)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as I
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int

sepUnder = T.pack "_"
sepSpace = T.pack " "

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: T.Text -> (T.Text, [T.Text])
parseFile line = (head wl, tail wl)
  where
    wl = T.words line

largeprime = 109297

hash' :: (Word, Word) -> Word -> Word
hash' (n1, n2) h = (n1*h + n2) `rem` largeprime

word2byte = toLazyText . decimal

-- |'lsh' generates 'r' LSH signatures with 'b' hashes each
lsh :: Int -> Int -> [(Word,Word)] -> (T.Text, [T.Text]) -> [T.Text]
lsh r b rnd (label, ws) = map output lshSigns
  where
    output signature = T.intercalate sepSpace [signature, label]
    lshSigns         = sort $ map (T.intercalate sepUnder) lshList
    lshList          = map addIdx $ zip [1..] (Sp.chunksOf b minhash)
    addIdx (idx, xs) = word2byte idx : xs
    minhash          = map genSignature rnd
    genSignature n   = word2byte $ minimum $ map (hash' n) hashedWords
    hashedWords      = map (\w -> fromIntegral (hash w) :: Word) ws

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

      (g1, g2) = split g
      
      n1 = take (r*b) (randomRs (1, largeprime-1) g1)
      n2 = take (r*b) (randomRs (1, largeprime-1) g2)
      rnd = zip n1 n2
      lsh' = lsh r b rnd

    content <- I.readFile fileIn
    let output = T.unlines 
               $ map T.unlines 
               $ map lsh'
               $ map parseFile 
               $ T.lines content
    I.writeFile fileOut output
