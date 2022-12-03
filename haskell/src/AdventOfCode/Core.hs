module AdventOfCode.Core
  ( Puzzle (..)
  , runPuzzle
  , runPuzzleIO

  , StreamingPuzzle (..)
  , runStreamingPuzzle
  , runStreamingPuzzleIO

    -- * Re-exports
  , SerialT
  , Fold
  , MonadThrow
  , MonadCatch
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow, tryAny, throw)
import Relude
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (SerialT)

import qualified Data.Text.IO as Text
import qualified Streamly.Prelude as Streamly
import qualified System.IO as IO

data Puzzle = forall i o. Show o => Puzzle
  { puzzle_parse :: Text -> Either Text i
  , puzzle_solve :: i -> Either Text o
  }

runPuzzle :: Puzzle -> Text -> Either Text Text
runPuzzle (Puzzle parse solve) input =
  parse input >>= solve <&> show

runPuzzleIO :: MonadIO m => Puzzle -> m ()
runPuzzleIO puzzle = liftIO do
  input <- Text.getContents
  case runPuzzle puzzle input of
    Left err -> Text.hPutStrLn stderr err *> exitFailure
    Right answer -> putTextLn answer

data StreamingPuzzle = forall i o. Show o => StreamingPuzzle
  { streamingPuzzle_parse :: forall m. MonadThrow m => SerialT m Text -> SerialT m i
  , streamingPuzzle_solve :: forall m. MonadThrow m => Fold m i o
  }

runStreamingPuzzle
  :: MonadCatch m
  => StreamingPuzzle
  -> SerialT m Text
  -> m (Either SomeException Text)
runStreamingPuzzle (StreamingPuzzle parse solve) input = do
  input
  & parse
  & Streamly.fold solve
  & fmap show
  & tryAny

runStreamingPuzzleIO :: MonadIO m => StreamingPuzzle -> m ()
runStreamingPuzzleIO puzzle = liftIO do
  runStreamingPuzzle puzzle (hGetLines stdin) >>= \case
    Left exc -> throw exc
    Right answer -> putTextLn answer

hGetLines
  :: MonadIO (t m)
  => Streamly.IsStream t
  => Streamly.MonadAsync m
  => Handle
  -> t m Text
hGetLines handle = do
  liftIO $ IO.hSetBuffering handle IO.LineBuffering
  flip Streamly.unfoldrM () \() -> liftIO do
    eof <- IO.hIsEOF handle
    if eof then
      pure Nothing
    else do
      line <- Text.hGetLine handle
      pure $ Just (line, ())
