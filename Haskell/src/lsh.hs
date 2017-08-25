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

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Builder

sepUnder = C.pack "_"
sepSpace = C.pack " "

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: B.ByteString -> (B.ByteString, [B.ByteString])
parseFile line = (head wl, tail wl)
  where
    wl = C.words line

largeprime = 109297

hash' :: (Word, Word) -> Word -> Word
hash' (n1, n2) h = (n1*h + n2) `rem` largeprime

word2byte = toLazyByteString . wordDec

-- |'lsh' generates 'r' LSH signatures with 'b' hashes each
lsh :: Int -> Int -> [(Word,Word)] -> (B.ByteString, [B.ByteString]) -> [B.ByteString]
lsh r b rnd (label, ws) = map output lshSigns
  where
    output signature = B.intercalate sepSpace [signature, label]
    lshSigns         = sort $ map (B.intercalate sepUnder) lshList
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

      n1 = take (r*b) (randomRs (1, largeprime-1) g)
      n2 = take (r*b) (randomRs (1, largeprime-1) g)
      rnd = zip n1 n2
      lsh' = lsh r b rnd

    content <- B.readFile fileIn
    let output = C.unlines 
               $ map C.unlines 
               $ map lsh'
               $ map parseFile 
               $ C.lines content
    B.writeFile fileOut output
