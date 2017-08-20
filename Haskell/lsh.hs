{-|
Module      : lsh
Description : applies lsh to dyadic data set
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

applies LSH to categorical data for the first step of the HBLCoClust algorithm.
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

lsh :: Int -> Int -> [(Int,Int)] -> (String, [String]) -> [String]
lsh r b rnd (label, ws) = map (\l -> format "{0} {1}" [l, label]) lshList
  where
    lshList   = sort $ map (intercalate "_") $ map (\(x,y) -> show y : x) $ zip (Sp.chunksOf b minhash) [1..]
    minhash   = [show $ minimum $ map ((hash' n) . hash) ws | n <- rnd]
    hash' (n1,n2) h = (n1*h + n2) `mod` largeprime

largeprime = 109297

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    let fileIn = args !! 0
    let fileOut = args !! 1
    let r = read (args !! 2) :: Int
    let b = read (args !! 3) :: Int
    --runResourceT $ S.print $ S.map (\x -> map hash $ snd x) $ S.map parseFile $ S.readFile filename

    g <- getStdGen
    let n1 = take (r*b) (randomRs (1, largeprime-1) g)
    let n2 = take (r*b) (randomRs (1, largeprime-1) g)
    let rnd = zip n1 n2
    let lsh' = lsh r b rnd
    runResourceT $ S.writeFile fileOut $ S.map unlines $ S.map lsh' $ S.map parseFile $ S.readFile fileIn
    --print (lsh dataset r b n1 n2)
