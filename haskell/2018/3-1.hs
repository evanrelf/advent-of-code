{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Function ((&))
import Text.ParserCombinators.ReadP (char, munch1, readP_to_S, string)
import Prelude hiding (id)


data Claim = Claim
  { _id :: Int
  , _x :: Int
  , _y :: Int
  , _width :: Int
  , _height :: Int
  } deriving Show


type Fabric = [[Int]]


parseClaim :: String -> Claim
parseClaim = fst . head . readP_to_S
  (Claim
    <$> (char '#' *> int)
    <*> (string " @ " *> int)
    <*> (char ',' *> int)
    <*> (string ": " *> int)
    <*> (char 'x' *> int))
  where int = read <$> munch1 (`elem` ['0'..'9'])


parse :: String -> [Claim]
parse = fmap parseClaim . lines


modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt f i xs = take i xs <> [f (xs !! i)] <> drop (i + 1) xs


modifyAt2d :: (a -> a) -> Int -> Int -> [[a]] -> [[a]]
modifyAt2d f x y = modifyAt (modifyAt f x) y {- HLINT ignore "Eta reduce" -}


initialFabric :: Fabric
initialFabric = replicate 1000 (replicate 1000 0)


applyClaim :: Fabric -> Claim -> Fabric
applyClaim fabric Claim { _x, _y, _width, _height } =
  foldl (&) fabric (modifyAt2d (+ 1) <$> xs <*> ys)
  where xs = [_x .. ((_x - 1) + _width)]
        ys = [_y .. ((_y - 1) + _height)]


applyClaims :: Fabric -> [Claim] -> Fabric
applyClaims = foldl applyClaim


overlapping :: Fabric -> Int
overlapping = length . filter (>= 2) . concat


solve :: [Claim] -> Int
solve = overlapping . applyClaims initialFabric


main :: IO ()
main = print . solve . parse =<< getContents
