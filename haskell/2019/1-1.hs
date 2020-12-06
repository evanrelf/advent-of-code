{-# LANGUAGE TypeApplications #-}

calculateFuel :: Int -> Int
calculateFuel mass = floor @Double (realToFrac mass / 3) - 2

solve :: String -> Int
solve = sum . fmap (calculateFuel . read) . lines

main :: IO ()
main = print . solve =<< getContents
