module AdventOfCode.Optics where

import Data.Text qualified as Text
import Optics

lines :: IxFold Int Text Text
lines = ifolding Text.lines

chars :: IxTraversal' Int Text Char
chars = each

bookends :: Getter (NonEmpty a) (a, a)
bookends = to \xs -> (head xs, last xs)
