module Main (main) where


parse :: String -> Int
parse ('+':is) = read is
parse ('-':is) = negate (read is)
parse _ = error "Invalid string"


calibrate :: [Int] -> Int
calibrate [] = error "Started with empty list"
calibrate cs = go [0] 0 cs where
  go history current [] = go history current cs
  go history current (change:rest) =
    let new = current + change in
    if new `elem` history
      then new
      else go (new:history) new rest


main :: IO ()
main = print . calibrate . fmap parse . lines =<< getContents
