import Control.Monad (guard)
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)

type MemoryRef s = STRef s [Integer]

type PositionRef s = STRef s Int

type Context s = (MemoryRef s, PositionRef s)

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f xs = take i xs <> [f (xs !! i)] <> drop (i + 1) xs

parseInput :: String -> [Integer]
parseInput input = read ("[" <> input <> "]")

get :: MemoryRef s -> Int -> ST s Integer
get ref index = (!! index) <$> readSTRef ref

put :: MemoryRef s -> Int -> Integer -> ST s ()
put ref index value = modifySTRef ref (modifyAt index (const value))

step :: PositionRef s -> ST s ()
step positionRef = modifySTRef positionRef (+ 4)

runOperation :: MemoryRef s -> Int -> (Integer -> Integer -> Integer) -> ST s ()
runOperation memoryRef position fn = do
  x <- get memoryRef . fromInteger =<< get memoryRef (position + 1)
  y <- get memoryRef . fromInteger =<< get memoryRef (position + 2)
  out <- get memoryRef (position + 3)
  put memoryRef (fromInteger out) (fn x y)

run :: Context s -> ST s Integer
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

solve :: (Integer, Integer) -> [Integer] -> Integer
solve (noun, verb) xs = runST $ do
  memoryRef <- newSTRef xs
  positionRef <- newSTRef 0
  let context = (memoryRef, positionRef)
  put memoryRef 1 noun
  put memoryRef 2 verb
  run context

bruteForce :: [Integer] -> Integer
bruteForce xs = head $ do
  noun <- [1 .. 99]
  verb <- [1 .. 99]
  let output = solve (noun, verb) xs
  guard (output == 19690720)
  pure (100 * noun + verb)

main :: IO ()
main = print . bruteForce . parseInput =<< getContents
