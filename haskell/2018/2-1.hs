count :: (a -> Bool) -> [a] -> Int
count pred = foldl (\acc x -> if pred x then acc + 1 else acc) 0

hasTwo :: String -> Bool
hasTwo str = 2 `elem` counts where
  counts = fmap (\char -> count (== char) str) str

hasThree :: String -> Bool
hasThree str = 3 `elem` counts where
  counts = fmap (\char -> count (== char) str) str

checksum :: [String] -> Int
checksum ids = count hasTwo ids * count hasThree ids

main :: IO ()
main = print . checksum . lines =<< getContents
