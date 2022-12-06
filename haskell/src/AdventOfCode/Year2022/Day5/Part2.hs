module AdventOfCode.Year2022.Day5.Part2 (main) where

import AdventOfCode.Core
import AdventOfCode.Parsing
import Relude hiding (some)

import qualified Data.IntMap.Strict as IntMap
import qualified Relude.Unsafe as Unsafe

data Step = Step
  { quantity :: Int
  , from :: Int
  , to :: Int
  }
  deriving stock (Show)

parse :: Text -> Either Text (IntMap [Char], [Step])
parse = runParser do
  let number = Unsafe.read <$> some digitChar

  let crate = do
        _ <- char '['
        letter <- satisfy (`elem` ['A' .. 'Z'])
        _ <- char ']'
        pure letter

  let maybeCrate = (Just <$> crate) <|> (string "   " $> Nothing)

  let crateRow = maybeCrate `sepBy1` char ' '

  crateRows <- crateRow `endBy1` newline

  let crateStacks = catMaybes <$> transpose crateRows

  stackNumbers <- hspace1 *> number `endBy1` hspace1 <* newline

  let stacks = IntMap.fromList (zip stackNumbers crateStacks)

  _ <- newline

  let step = do
        _ <- string "move "
        quantity <- number
        _ <- string " from "
        from <- number
        _ <- string " to "
        to <- number
        pure Step{ quantity, from, to }

  steps <- step `sepBy` newline

  eof

  pure (stacks, steps)

move :: Step -> IntMap [Char] -> IntMap [Char]
move Step{ quantity, from, to } stacks =
  stacks
  & IntMap.insert from stationary
  & IntMap.insertWith (<>) to moving
  where
  (moving, stationary) = splitAt quantity (stacks IntMap.! from)

solve :: (IntMap [Char], [Step]) -> Either Text [Char]
solve (stacks, steps) =
  foldl' (flip move) stacks steps
  & IntMap.elems
  & fmap Unsafe.head
  & Right

main :: IO ()
main = run $ BasicPuzzle parse solve
