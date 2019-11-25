{-# LANGUAGE NamedFieldPuns #-}

import Prelude hiding (id)

import Debug.Trace

import Data.Function ((&))
import Text.ParserCombinators.ReadP

type Fabric = [[Int]]

data Claim = Claim
  { id :: Int
  , x :: Int
  , y :: Int
  , w :: Int
  , h :: Int
  } deriving Show

digit :: ReadP Char
digit = satisfy (`elem` ['0'..'9'])

int :: ReadP Int
int = read <$> many1 digit

claim :: ReadP Claim
claim = do
  _ <- char '#'
  id <- int
  _ <- string " @ "
  x <- int
  _ <- char ','
  y <- int
  _ <- string ": "
  w <- int
  _ <- char 'x'
  h <- int
  pure Claim { id, x, y, w, h }

parse :: String -> Claim
parse = fst . head . readP_to_S claim

initialFabric :: Fabric
initialFabric = replicate 1000 (replicate 1000 0)

modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt f i xs = take i xs <> [f (xs !! i)] <> drop (i + 1) xs

modifyAt2d :: (a -> a) -> Int -> Int -> [[a]] -> [[a]]
modifyAt2d f x y = modifyAt (modifyAt f x) y

applyClaim :: Fabric -> Claim -> Fabric
applyClaim fabric Claim { x, y, w, h } = foldl (&) fabric modifications
  where
    xs = [x .. (x + (w - 1))]
    ys = [y .. (y + (h - 1))]
    modifications = modifyAt2d (+1) <$> xs <*> ys

applyClaims :: Fabric -> [Claim] -> Fabric
applyClaims fabric = foldl applyClaim fabric

countOverlapping :: Fabric -> Int
countOverlapping = length . filter (> 1) . concat

main :: IO ()
main = print
     . countOverlapping
     . applyClaims initialFabric
     . fmap parse
     . lines
   =<< getContents

{- HLINT ignore "Eta reduce" -}
