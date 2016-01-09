{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module TextFile.Template.QuoteString(
    QuoteString(
        toString, length, tails, inits, splitAt, splitOn, breakOn,
        stripPrefix, stripSuffix, isPrefixOf, isSuffixOf),
    lines, unlines
) where

import Prelude hiding (length, splitAt, lines, unlines)

import Data.Maybe     (isJust)
import Data.String    (IsString(fromString))
import Data.Bifunctor (Bifunctor(bimap))

import qualified Data.List             as List
import qualified Data.Text             as Text
import qualified Data.ByteString.Char8 as ByteString

--------------------------------------------------------------------------------
-- The class of String-like types which can be generated from a string template.
-- Instances are equipped with conversion to and from String; Eq and Monoid
-- instances agreeing with this correspondence; and several standard string-
-- manipulation functions which may be replaced with efficient native versions.
--
-- Instances should satisfy the following laws, where f = toString . fromString:
--  1. x == y                   = toString x == toString y forall x,y :: a
--  2. toString (x `mconcat` y) = toString x ++ toString y forall x,y :: a
--  3. f (toString x)           = toString x               forall x   :: a
--  4. f (s ++ t)               = f s ++ f t               forall s,t :: String
--  5. tail (f [c])             = []                       forall c   :: Char
--  6. All other methods are equal modulo (==) to the default implementations,
--     whenever their arguments, if finite, have lengths representable as Ints.
--
-- Note that `toString (fromString t) == t' is not required to hold, as there
-- are admissable types, such as Data.ByteString.Char8.ByteString, which discard
-- some information when converting from a string.
--
-- A complete definition can be achieved by implementing only `toString',
-- however it is recommended for efficiency to implement at least
--    toString, length, tails, inits, and splitAt,
-- as the default implementations are for documentation and not efficient.
--------------------------------------------------------------------------------

class (Eq s, Monoid s, IsString s) => QuoteString s where
    -- The list of characters in this string.
    toString :: s -> String

    -- The number of characters in the string, truncated to fit in an Int.
    length :: s -> Int
    length = List.length . toString

    -- A list whose first element is the given string, each subsequent
    -- element the result of removing one character from the beginning or end,
    -- respectively, of the previous element, and last element the empty string.
    tails, inits :: s -> [s]
    tails = map fromString . List.tails . toString
    inits = map fromString . List.inits . toString

    -- The two substrings occurring before and after the given index, which is
    -- zero-based, but may be < 0 or >= the length of this string. The second
    -- substring includes the character at that index, if there is one.
    splitAt :: Int -> s -> (s, s)
    splitAt i = bimap fromString fromString . List.splitAt i . toString

    -- The two substrings occurring before and after the first occurrence of the
    -- given substring, where the second result includes the input substring;
    -- or (str, "") if the substring does not occur anywhere in this string.
    breakOn :: s -> s -> (s, s)
    breakOn sub str
      = foldr f (str, "") (zip (inits str) (tails str))
      where f (pre, suf) next | sub `isPrefixOf` suf = (pre, suf)
                              | otherwise            = next

    -- The list of substrings separated by occurrences of the given substring,
    -- including empty strings, but not including the separator.
    splitOn :: s -> s -> [s]
    splitOn sub str = case stripPrefix sub suf of
        Just suf' -> pre : splitOn sub suf'
        Nothing   -> [pre]
      where (pre, suf) = breakOn sub str

    -- If the given prefix occurs at the beginning of this string, Just the string
    -- with the prefix removed; otherwise, Nothing. Useful with -XViewPatterns.
    stripPrefix :: s -> s -> Maybe s
    stripPrefix pre str
      | pre == pre' = Just suf
      | otherwise   = Nothing
      where (pre', suf) = splitAt (length pre) str

    -- If the given suffix occurs at the end of this string, Just the string
    -- with this suffix removed; otherwise, Nothing. Useful with -XViewPatterns.
    stripSuffix :: s -> s -> Maybe s
    stripSuffix suf str
      | suf == suf' = Just pre
      | otherwise   = Nothing
      where (pre, suf') = splitAt (length str - length suf) str

    -- True if the given substring occurs at the beginning or, respectively,
    -- the end of the string; otherwise, False.
    isPrefixOf, isSuffixOf :: s -> s -> Bool
    pre `isPrefixOf` str = isJust $ stripPrefix pre str
    suf `isSuffixOf` str = isJust $ stripSuffix suf str

instance QuoteString Text.Text where
    toString    = Text.unpack
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
    toString    = id
    length      = List.length
    tails       = List.tails
    inits       = List.inits
    splitAt     = List.splitAt
    stripPrefix = List.stripPrefix
    isPrefixOf  = List.isPrefixOf
    isSuffixOf  = List.isSuffixOf

instance QuoteString ByteString.ByteString where
    toString    = ByteString.unpack
    length      = ByteString.length
    tails       = ByteString.tails
    inits       = ByteString.inits
    splitAt     = ByteString.splitAt
    breakOn     = ByteString.breakSubstring
    isPrefixOf  = ByteString.isPrefixOf
    isSuffixOf  = ByteString.isSuffixOf

--------------------------------------------------------------------------------
-- Polymorphic versions of Prelude.lines and Prelude.unlines, with slightly
-- different semantics regarding newlines. For any legal instance of
-- QuoteString, it holds that `unlines . lines = id'.

-- Split the given string into a list of substrings separated by newlines, not
-- including the newline characters, but including any empty line at the end.
lines :: QuoteString s => s -> [s]    
lines = splitOn "\n"

-- Join the given list of strings by concatenating them, after:
-- 1. Appending '\n' to each line, except the last, not already ending in one.
-- 2. Removing a single '\n' from the end of the last string, if there is one.
unlines :: QuoteString s => [s] -> s
unlines = mconcat . foldr f [] where
    f line xs = case (stripSuffix "\n" line, xs) of
        (Nothing,    [])  -> [line]
        (Nothing,    _:_) -> line : "\n" : xs
        (Just line', [])  -> [line']
        (Just _,     _:_) -> line : xs
