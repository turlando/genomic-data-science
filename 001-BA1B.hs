-- Find the Most Frequent Words in a String
-- http://rosalind.info/problems/ba1b/
--
-- We say that Pattern is a most frequent k-mer in Text if it maximizes
-- Count(Text, Pattern) among all k-mers. For example, "ACTAT" is a most
-- frequent 5-mer in "ACAACTATGCATCACTATCGGGAACTATCCT", and "ATA" is a
-- most frequent 3-mer of "CGATATATCCATAG".
--
-- Find the most frequent k-mers in a string.
-- Given: A DNA string Text and an integer k.
-- Return: All most frequent k-mers in Text (in any order).

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Text as Text

import Data.Map  (Map)
import Data.Text (Text)

type Frequencies = Map Text Int
type Frequency   = (Text, Int)

doit :: Text -> Int -> (Int, [Text])
doit s n = Text.intercalate " " $ snd $ mostFrequent $ frequencies s n

frequencies :: Text -> Int -> Frequencies
frequencies s n = go s n Map.empty
  where
    go :: Text -> Int -> Frequencies -> Frequencies
    go s n m = let s' = Text.take n s
               in if Text.length s' == n
                  then go (Text.tail s) n (upsert m s')
                  else m

upsert :: Frequencies -> Text -> Frequencies
upsert m k = Map.insertWith (+) k 1 m

mostFrequent :: Frequencies -> (Int, [Text])
mostFrequent m = clean $ last $ List.groupBy comparator $ sortFrequencies m
  where
    comparator :: Frequency -> Frequency -> Bool
    comparator (s1, n1) (s2, n2) = n1 == n2
    clean :: [Frequency] -> (Int, [Text])
    clean l = (snd $ head l, fmap fst l)

sortFrequencies :: Frequencies -> [Frequency]
sortFrequencies m = List.sortBy comparator $ Map.toList m
  where
    comparator :: Frequency -> Frequency -> Ordering
    comparator (s1, n1) (s2, n2) = compare n1 n2
