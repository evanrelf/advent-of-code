module AdventOfCode.Core
  ( Puzzle (..)

  , BasicPuzzle (..)
  , runBasicPuzzle
  , runBasicPuzzleIO

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

import Control.Exception.Safe (MonadCatch, MonadThrow, throw, tryAny)
import Relude
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (SerialT)

import qualified Data.Text.IO as Text
import qualified Streamly.Prelude as Streamly
import qualified System.IO as IO

class Puzzle a where
  run :: MonadIO m => a -> m ()

data BasicPuzzle = forall i o. Show o => BasicPuzzle
  { basicPuzzle_parse :: Text -> Either Text i
  , basicPuzzle_solve :: i -> Either Text o
  }

runBasicPuzzle :: BasicPuzzle -> Text -> Either Text Text
runBasicPuzzle (BasicPuzzle parse solve) input =
  parse input >>= solve <&> show

runBasicPuzzleIO :: MonadIO m => BasicPuzzle -> m ()
runBasicPuzzleIO puzzle = liftIO do
  input <- Text.getContents
  case runBasicPuzzle puzzle input of
    Left err -> Text.hPutStrLn stderr err *> exitFailure
    Right answer -> putTextLn answer

instance Puzzle BasicPuzzle where
  run = runBasicPuzzleIO

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

instance Puzzle StreamingPuzzle where
  run = runStreamingPuzzleIO

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
