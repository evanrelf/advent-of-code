#!/usr/bin/env nix-shell
#!nix-shell --pure --packages ghc -i runghc

parse :: String -> Int
parse ('+':is) = read is
parse ('-':is) = negate (read is)
parse _ = error "Invalid string"

main :: IO ()
main = print . sum . fmap parse . lines =<< getContents
