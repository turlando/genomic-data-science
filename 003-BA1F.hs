-- Find a Position in a Genome Minimizing the Skew
-- http://rosalind.info/problems/ba1f/
--
-- Define the skew of a DNA string Genome, denoted Skew(Genome), as the
-- difference between the total number of occurrences of 'G' and 'C' in
-- Genome. Let Prefixi (Genome) denote the prefix (i.e., initial substring)
-- of Genome of length i. For example, the values of
-- Skew(Prefix_i("CATGGGCATCGGCCATACGCC")) are:
-- 0 -1 -1 -1 0 1 2 1 1 1 0 1 2 1 0 0 0 0 -1 0 -1 -2
--
-- Find a position in a genome minimizing the skew.
-- Given: A DNA string Genome.
-- Return: All integer(s) i minimizing Skew(Prefix_i(Text)) over all values
-- of i (from 0 to |Genome|).

import qualified Data.List as List

type Genome       = String
type Skew         = Int
type Skews        = [Int]
type Position     = Int
type Positions    = [Position]
type IndexedSkew  = (Position, Skew)
type IndexedSkews = [IndexedSkew]

doit :: Genome -> String
doit s = List.intercalate " "
         $ fmap show
         $ findMinSkewPositions
         $ zipSkews
         $ skews s

skews :: Genome -> Skews
skews string = List.tail $ reverse $ go string [0]
  where
    go :: Genome -> Skews -> Skews
    go ""         acc      = acc
    go ('G' : xs) (y : ys) = go xs (y + 1 : y : ys)
    go ('C' : xs) (y : ys) = go xs (y - 1 : y : ys)
    go (_   : xs) (y : ys) = go xs (y     : y : ys)

zipSkews :: Skews -> IndexedSkews
zipSkews skews = zip [1..] skews

findMinSkewPositions :: IndexedSkews -> Positions
findMinSkewPositions ((i0, s0) : xs) = reverse $ go xs (i0, s0) [i0]
  where
    go :: IndexedSkews -> IndexedSkew -> Positions -> Positions
    go []              _        acc = acc
    go ((i1, s1) : xs) (i0, s0) acc
      | s0 < s1  = go xs (i0, s0) acc
      | s0 == s1 = go xs (i0, s0) (i1 : acc)
      | s0 > s1  = go xs (i1, s1) [i1]
