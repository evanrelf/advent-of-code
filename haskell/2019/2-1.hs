import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)

type MemoryRef s = STRef s [Int]

type PositionRef s = STRef s Int

type Context s = (MemoryRef s, PositionRef s)

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = take i xs <> [f (xs !! i)] <> drop (i + 1) xs

parseInput :: String -> [Int]
parseInput input = read ("[" <> input <> "]")

get :: MemoryRef s -> Int -> ST s Int
get ref index = (!! index) <$> readSTRef ref

put :: MemoryRef s -> Int -> Int -> ST s ()
put ref index value = modifySTRef ref (modifyAt index (const value))

step :: PositionRef s -> ST s ()
step positionRef = modifySTRef positionRef (+ 4)

runOperation :: MemoryRef s -> Int -> (Int -> Int -> Int) -> ST s ()
runOperation memoryRef position fn = do
  x <- get memoryRef =<< get memoryRef (position + 1)
  y <- get memoryRef =<< get memoryRef (position + 2)
  out <- get memoryRef (position + 3)
  put memoryRef out (fn x y)

run :: Context s -> ST s Int
run context@(memoryRef, positionRef) = do
  position <- readSTRef positionRef
  operation <- get memoryRef position
  case operation of
    1 -> do
      -- Add
      runOperation memoryRef position (+)
      step positionRef
      run context
    2 -> do
      -- Multiply
      runOperation memoryRef position (*)
      step positionRef
      run context
    99 ->
      -- Halt
      get memoryRef 0
    invalid ->
      error ("Invalid operation " <> show invalid <> " at position " <> show position)

solve :: [Int] -> Int
solve xs = runST $ do
  memoryRef <- newSTRef xs
  positionRef <- newSTRef 0
  let context = (memoryRef, positionRef)
  put memoryRef 1 12
  put memoryRef 2 2
  run context

main :: IO ()
main = print . solve . parseInput =<< getContents
