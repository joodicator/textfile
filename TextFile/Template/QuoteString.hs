{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module TextFile.Template.QuoteString(
    QuoteString(length, tails, inits, splitAt, splitOn, breakOn,
                stripPrefix, stripSuffix, isPrefixOf, isSuffixOf),
    lines, unlines
) where

import Prelude hiding (length, splitAt, lines, unlines)

import Data.Maybe  (isJust)
import Data.String (IsString)

import qualified Data.List             as List
import qualified Data.Text             as Text
import qualified Data.ByteString.Char8 as ByteString

--------------------------------------------------------------------------------
class (Eq s, Monoid s, IsString s) => QuoteString s where
    length       :: s -> Int
    tails, inits :: s -> [s]
    splitAt      :: Int -> s -> (s, s)

    breakOn :: s -> s -> (s, s)
    breakOn sub str
      = foldr f (str, "") (zip (inits str) (tails str))
      where f (pre, suf) next | sub `isPrefixOf` suf = (pre, suf)
                              | otherwise            = next

    splitOn :: s -> s -> [s]
    splitOn sub str = case stripPrefix sub suf of
        Just suf' -> pre : splitOn sub suf'
        Nothing   -> [pre]
      where (pre, suf) = breakOn sub str

    stripPrefix :: s -> s -> Maybe s
    stripPrefix pre str
      | pre == pre' = Just suf
      | otherwise   = Nothing
      where (pre', suf) = splitAt (length pre) str

    stripSuffix :: s -> s -> Maybe s
    stripSuffix suf str
      | suf == suf' = Just pre
      | otherwise   = Nothing
      where (pre, suf') = splitAt (length str - length suf) str

    isPrefixOf, isSuffixOf :: s -> s -> Bool
    pre `isPrefixOf` str = isJust $ stripPrefix pre str
    suf `isSuffixOf` str = isJust $ stripSuffix suf str

instance QuoteString Text.Text where
    length      = Text.length
    tails       = Text.tails
    inits       = Text.inits
    splitAt     = Text.splitAt
    splitOn     = Text.splitOn
    breakOn     = Text.breakOn
    stripPrefix = Text.stripPrefix
    stripSuffix = Text.stripSuffix
    isPrefixOf  = Text.isPrefixOf
    isSuffixOf  = Text.isSuffixOf

instance QuoteString String where
    length      = List.length
    tails       = List.tails
    inits       = List.inits
    splitAt     = List.splitAt
    stripPrefix = List.stripPrefix
    isPrefixOf  = List.isPrefixOf
    isSuffixOf  = List.isSuffixOf

instance QuoteString ByteString.ByteString where
    length      = ByteString.length
    tails       = ByteString.tails
    inits       = ByteString.inits
    splitAt     = ByteString.splitAt
    breakOn     = ByteString.breakSubstring
    isPrefixOf  = ByteString.isPrefixOf
    isSuffixOf  = ByteString.isSuffixOf

--------------------------------------------------------------------------------
lines :: QuoteString s => s -> [s]    
lines = splitOn "\n"

unlines :: QuoteString s => [s] -> s
unlines = mconcat . List.intersperse "\n"
