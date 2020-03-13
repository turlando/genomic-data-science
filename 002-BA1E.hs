-- Find Patterns Forming Clumps in a String
-- http://rosalind.info/problems/ba1e/
--
-- Given integers L and t, a string Pattern forms an (L, t)-clump inside a
-- (larger) string Genome if there is an interval of Genome of length L in
-- which Pattern appears at least t times. For example, TGCA forms a
-- (25,3)-clump in the following Genome:
-- gatcagcataagggtcccTGCAATGCATGACAAGCCTGCAgttgttttac.
--
-- Find patterns forming clumps in a string.
-- Given: A string Genome, and integers k, L, and t.
-- Return: All distinct k-mers forming (L, t)-clumps in Genome.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.Text as Text

import Data.Set  (Set)
import Data.Map  (Map)
import Data.Text (Text)

type Genome      = Text
type Length      = Int
type Pattern     = Text
type Count       = Int
type Frequencies = Map Pattern Count
type Patterns    = Set Pattern
type Range       = Int

doit :: Genome -> Length -> Range -> Count -> Text
doit string length range count = Text.intercalate " "
                                 $ Set.toList
                                 $ clumps string length range count

frequencies :: Genome -> Length -> Frequencies
frequencies s n = go s Map.empty
  where
    go :: Text -> Frequencies -> Frequencies
    go s acc = let s' = Text.take n s
               in if Text.length s' == n
                  then go (Text.tail s) (upsert acc s')
                  else acc
    upsert :: Frequencies -> Pattern -> Frequencies
    upsert m k = Map.insertWith (+) k 1 m

frequentAtLeast :: Genome -> Length -> Count -> Patterns
frequentAtLeast string length count = Map.keysSet
                                      $ Map.filter (>= count)
                                      $ frequencies string length

-- O(n²) but IMHO can be optimised to O(nlogn)
clumps :: Genome -> Length -> Range -> Count -> Patterns
clumps string length range count = go string Set.empty
  where
    go :: Genome -> Patterns -> Patterns
    go string acc =
      let substring = Text.take range string
      in if Text.length substring == range
         then go (Text.tail string)
                 (Set.union acc (frequentAtLeast string length count))
         else acc
