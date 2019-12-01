{-# LANGUAGE TypeApplications #-}

calculateFuel :: Int -> Int
calculateFuel mass
  | mass <= 0 = 0
  | otherwise =
    let f = floor @Double (realToFrac mass / 3) - 2
        fuel = if f <= 0 then 0 else f
     in fuel + calculateFuel fuel

solve :: String -> Int
solve = sum . fmap (calculateFuel . read) . lines

main :: IO ()
main = print . solve =<< getContents
