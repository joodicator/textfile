{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveFunctor, ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module TextFile.Template.Indent(indent) where

{-------------------------------------------------------------------------------
This module provides the quasiquoter [indent|...|], which allows the writing 
of multi-line string literals, containing interpolated Haskell expressions, 
within Haskell programs. For example, in GHCi:

    > :l TextFile.Template.Indent
    > :set -XQuasiQuotes
    > :{
    |     let x = "world" in [indent|
    |        Hello,
    |        :[x]:.
    |     |]
    | :}
    "Hello,\nworld.\n"

To use the quasiquoter, a module must be compiled with -XQuasiQuotes; then, an 
interpolated string expression is introduced by "[indent|", then a TEMPLATE 
possibly containing newlines but not containing "|]", then a terminating "|]". 
The quasiquote will be expanded during compilation to an expression of type 
`QuoteString a => a', or a specialisation thereof. This type may become 
polymorphic, monomorphic, or the cause of an "ambiguous type" error, depending 
on where it occurs in the code and on whether -XNoMonomorphismRestriction or 
-XOverloadedStrings are in effect (the literal parts of the quote are always 
polymorphic, but the latter may apply if regular string literals occur in 
anti-quoted code in the template).

--------------------------------------------------------------------------------
    Syntax
--------------------------------------------------------------------------------
A TEMPLATE consists of LITERAL plain text characters, punctuated by:
 1. ESCAPE SEQUENCEs having any of the following forms:
     a. ":~[" or "]~:", which is decoded as ":[" or "]:", respectively, or
     b. "[~:" or ":~]", which is decoded as "[:" or ":]", respectively, or
     c. "[~|" or "|~]", which is decoded as "[|" or "|]", respectively; and
 2. ANTIQUOTEs, which consist of the string ":[", followed by some CODE to
    be evaluated as a Haskell expression, followed by the string "]:".

An anti-quoted CODE segment consists of plain Haskell syntax punctuated by:
 1. ESCAPE SEQUENCEs as defined above; and
 2. Sub-quasiquotes, consisting of the string "[:", followed by a TEMPLATE,
    followed by the string ":]", with the same meaning that this template would
    have in normal Haskell code if inside an [indent|...|] quote.

--------------------------------------------------------------------------------
    Semantics
--------------------------------------------------------------------------------
A TEMPLATE is evaluated as the concatenation of the values of its components,
after the following operations have been applied in order:

 1. Define the DEDENT as the longest contiguous string of spaces and tabs
    directly after the first LITERAL newline. (This is the indentation
    level relative to which which the template is assumed to be laid out.)
    After each LITERAL newline in the template, the longest prefix of the
    the DEDENT occurring at this point is deleted.

 2. For each ANTIQUOTE, consider the concatenated the values of the longest
    contiguous string of LITERALs and ESCAPE SEQUENCEs after the latest
    LITERAL newline in the template that precedes this antiquote; or
    consider the empty string if there is no such newline. Define the INDENT
    of this antiquote to be this string with each non-tab character replaced
    by a space. (This is the indentation level to which newlines generated
    by the antiquote will return.) Modify the value of each antiquote by
    inserting its INDENT after each newline in its original value.

 3. If the template starts with a LITERAL newline, delete the newline.

A CODE segment is evaluated in the lexical scope of the containing quote, and 
as if it were enclosed in square brackets, as a list literal or a list 
comprehension. The list is converted to a string by appending a newline to each
element not already terminated with one, except for the last element, from which
any single terminating newline is removed; and concatenating all elements. The
type of the list must be an instance of `QuoteString a => [a]', and must unify
with the types arising from any other code segments in the same quote.

-------------------------------------------------------------------------------}
import qualified Data.Map.Strict               as M
import qualified Language.Haskell.TH           as TH
import qualified Language.Haskell.TH.Quote     as TH
import qualified TextFile.Template.QuoteString as QS

import Data.Maybe                  (fromMaybe)
import Data.List                   (isInfixOf, stripPrefix)
import Data.Monoid                 ((<>))
import Data.String                 (fromString)
import Data.Data                   (Data(gmapM))
import Data.Typeable               (cast)
import Language.Haskell.Meta.Parse (parseExp)

--------------------------------------------------------------------------------
-- The [indent|...|] quasiquoter, as described at the beginning of this file.
indent :: TH.QuasiQuoter
indent = TH.QuasiQuoter{
    TH.quoteExp  = runQuote,
    TH.quotePat  = quoteFail "indent" "pattern",
    TH.quoteType = quoteFail "indent" "type",
    TH.quoteDec  = quoteFail "indent" "declaration" }

-- Generate a Haskell expression from the given quasiquoted string.
-- This involves three stages: parsing, enforcing indentation rules, and finally
-- writing out the result as standard Haskell syntax.
runQuote :: String -> TH.Q TH.Exp
runQuote = writeQuote . indentQuote . parseQuote

-- Raise a compiler error when the quasiquoter is used in an invalid context.
quoteFail :: String -> String -> String -> TH.Q a
quoteFail name context _
  = fail $ "Error: ["++name++"|...|] cannot be used in a "++context++" context."

--------------------------------------------------------------------------------
-- The syntax tree of a quasiquoted string, each node of which is annotated with
-- a value of type `a' if it has a representation as a Haskell expression.
--
-- When `a' is `()', the tree has no annotation, and could be used as a plain
-- parse tree; otherwise, it can be used to attach semantic information not
-- otherwise representable.
--
-- The Functor and Show instances are useful for manual inspection of trees:
-- `() <$ t' always has a Show instance when t :: Quote a, regardless of a.
data Quote a
  = Quote a (Tmpl a)
  deriving (Show, Functor)

-- The syntax tree of a template: the contents of a (sub-)quasiquote.
type Tmpl a = [TmplPart a]
data TmplPart a
  = TmplLiteral a String -- A segment of literal text.
  | AntiQuote a (Code a) -- An embedded Haskell expression.
  deriving (Show, Functor)

-- The syntax tree of a Haskell code segment: the contents of an antiquote.
type Code a = [CodePart a]
data CodePart a
  = CodeLiteral String  -- A segment of literal Haskell code.
  | SubQuote a (Tmpl a) -- An embedded string template.
  deriving (Show, Functor)

-- A type used to annotate quote syntax trees: each annotated node is associated
-- with a function to be used to generate its final representatation as a
-- Haskell expression, given some initial representation derived from the node.
type W = TH.Q TH.Exp -> TH.Q TH.Exp

--------------------------------------------------------------------------------
-- This section contains functions for parsing strings into parse trees.
-- A parser producing a result of type `a' is a function of type
--   String ->        a          -- if all input is consumed, or
--   String ->       (a, String) -- if some unconsumed input may be returned, or
--   String -> Maybe (a, String) -- if the parse might fail, producing no value.
--
-- The parsers here are implemented using helper functions, `parse', which make
-- use of /tail recursion/ and /accumulation passing style/. The accumulating
-- parameter, `acc', builds up a segment of literal text (the string is built
-- backwards, by necessity), which is finally pushed onto the result, using
-- `flush', when a non-literal production is encountered or the parse finishes.
-- This is done to avoid producing many single-character strings in the result.
--
-- Note: these parsers could have been implemented somewhat more succinctly
-- using a Monadic parser combinator library like Parsec (or AttoParsec, if
-- performance is important enough to compromise on flexibility), but in this
-- case the structures being parsed are simple enough that it was not considered
-- worth the extra dependency.

-- Parse an quasiquoted template into its syntax tree.
parseQuote :: String -> Quote ()
parseQuote = Quote () . parse "" where
    -- Escape sequence.
    parse acc (parseEscape -> Just (esc, str)) = parse (reverse esc ++ acc) str
    -- Antiquote.
    parse acc (stripPrefix ":[" -> Just str)
      = flush acc ++ AntiQuote () code : parse "" str'
      where (code, str') = parseAntiQuote str
    -- Literal character.
    parse acc (c:str) = parse (c:acc) str
    -- End of template.
    parse acc "" = flush acc

    flush ""  = []
    flush acc = [TmplLiteral () $ reverse acc]

-- Possibly parse an escape sequence into the string it represents.
parseEscape :: String -> Maybe (String, String)
parseEscape str = case str of
    (stripPrefix ":~[" -> Just str') -> Just (":[", str')
    (stripPrefix ":~]" -> Just str') -> Just (":]", str')
    (stripPrefix "|~]" -> Just str') -> Just ("|]", str')
    (stripPrefix "]~:" -> Just str') -> Just ("]:", str')
    (stripPrefix "[~:" -> Just str') -> Just ("[:", str')
    (stripPrefix "[~|" -> Just str') -> Just ("[|", str')
    _                                -> Nothing

-- Parse an anti-quoted code segment, starting after ":[", into its syntax tree.
parseAntiQuote :: String -> (Code (), String)
parseAntiQuote = parse "" where
    -- Escape sequence.
    parse acc (parseEscape -> Just (esc, str)) = parse (reverse esc ++ acc) str
    -- Sub-quasiquote.
    parse acc (stripPrefix "[:" -> Just str)
      = (flush acc ++ SubQuote () text : parts, str'')
      where (text, str')   = parseSubQuote str
            (parts, str'') = parseAntiQuote str'
    -- End of antiquote.
    parse acc (stripPrefix "]:" -> Just str) = (flush acc, str)
    -- Literal character.
    parse acc (c:str) = parse (c:acc) str   
    -- Erroneous end of string.
    parse _ "" = error "Parse error: unexpected end of string."

    flush ""  = []
    flush acc = [CodeLiteral $ reverse acc]

-- Parse a sub-quasiquoted template, starting after "[:", into its syntax tree.
parseSubQuote :: String -> (Tmpl (), String)
parseSubQuote = parse "" where
    -- Escape sequence.
    parse acc (parseEscape -> Just (esc, str)) = parse (reverse esc ++ acc) str
    -- Antiquote.
    parse acc (stripPrefix ":[" -> Just str)
      = (flush acc ++ AntiQuote () code : parts, str'')
      where (code, str')   = parseAntiQuote str
            (parts, str'') = parseSubQuote str'
    -- End of sub-quasiquote.
    parse acc (stripPrefix ":]" -> Just str) = (flush acc, str)
    -- Literal character.
    parse acc (c:str) = parse (c:acc) str
    -- Erroneous end of string.
    parse _ "" = error "Parse error: unexpected end of string."

    flush ""  = []
    flush acc = [TmplLiteral () $ reverse acc]

--------------------------------------------------------------------------------
-- The functions in this section enforce the semantic rules of the quasiquoter
-- by transforming plain parse trees into abstract syntax trees annotated with
-- functions of type W = TH.Q TH.Exp -> TH.Q TH.Exp.
--
-- Such a function encodes a transformation that the code generator should
-- apply to each expression as a final processing step. When no additional
-- processing is required, the identity function `id :: a -> a' is used.

type Indent = String -- A string of tabs/spaces used to indent lines.
type Dedent = String -- A string of tabs/spaces used to unindent lines.

-- Effect the indentation semantics on a quasiquoted template.
indentQuote :: Quote () -> Quote W
indentQuote (Quote _ tmpl)
  = Quote id (indentTmpl tmpl)

-- Effect the indentation semantics on a template.
indentTmpl :: Tmpl () -> Tmpl W
indentTmpl tmpl
  = case indentTmpl' "" dedent tmpl of
        TmplLiteral a ('\n':str) : tmpl' -> TmplLiteral a str : tmpl'
        tmpl'                            -> tmpl'
  where
    dedent = foldr f "" tmpl
    f (TmplLiteral _ (QS.lines -> _:l:_)) _ = takeWhile (`elem` [' ','\t']) l
    f _                                next = next

-- Effect the indentation semantics on some tail of a template, given both the
-- indent of the current line and the global dedent.
indentTmpl' :: Indent -> Dedent -> Tmpl () -> Tmpl W
indentTmpl' _ ded (TmplLiteral _ str : tmpl')
  = TmplLiteral id str' : indentTmpl' ind' ded tmpl'
  where
    str' = QS.unlines ls'
    ls'  = take 1 ls ++ map (stripLongestPrefix ded) (drop 1 ls)
    ls   = QS.lines str
    ind' = [if c /= '\t' then ' ' else c | l <- take 1 (reverse ls'), c <- l]
indentTmpl' ind ded (AntiQuote _ code : tmpl')
  = AntiQuote (indentCodeExp ind) (indentCode code) : indentTmpl' ind ded tmpl'
indentTmpl' _ _ [] = []

-- Effect the indentation semantics on an antiquoted code segment.
indentCode :: Code () -> Code W
indentCode = map $ \part -> case part of
    SubQuote _ tmpl -> SubQuote id (indentTmpl tmpl)
    CodeLiteral str -> CodeLiteral str

-- Modify a Haskell expression representing a string generated by an antiquote
-- by indenting each line, except the first, using the given string.
indentCodeExp :: Indent -> TH.Q TH.Exp -> TH.Q TH.Exp
indentCodeExp ind expr = [| indentCodeString ind $(expr) |]

-- Apply the given indent to each line, except the first, of the given string.
indentCodeString :: QS.QuoteString s => Indent -> s -> s
indentCodeString ind str
  = QS.unlines $ take 1 ls ++ map (fromString ind <>) (drop 1 ls)
  where ls = QS.lines str

--------------------------------------------------------------------------------
-- The functions in this section perform the final stage of conversion from an
-- annotated syntax tree into a pure Template Haskell expression respecting the
-- transformations encoded in its annotations.

-- The TH expression corresponding to the given quasiquoted template.
writeQuote :: Quote W -> TH.Q TH.Exp
writeQuote (Quote w parts) = w (writeTmpl parts)

-- The TH expression corresponding to the given template contents.
writeTmpl :: Tmpl W -> TH.Q TH.Exp
writeTmpl _parts = do
    _parts <- mapM writeTmplPart _parts
    case _parts of
        []      -> [| fromString "" |]
        [p]     -> [| $(return p) |]
        [p, p'] -> [| $(return p) <> $(return p') |]
        _parts  -> [| mconcat $(return $ TH.ListE _parts) |]

-- The TH expression corresponding to the given template element.
writeTmplPart :: TmplPart W -> TH.Q TH.Exp
writeTmplPart (TmplLiteral w str)  = w [| fromString str |]
writeTmplPart (AntiQuote   w code) = w (writeCode code)

-- The internal state of an ongoing evaluation of `writeCode'.
data WriteCode = WriteCode{
    -- The accumumulated code, including placeholder variables for sub-quotes.
    wcFullCode      :: String,
    -- The accumulated code, with " " in place of any placeholder variables.
    wcLiteralCode   :: String,
    -- The mapping from placeholder variable names to the values they represent.
    wcSubstitutions :: M.Map TH.Name (TH.Q TH.Exp),
    -- An infinite list of names suitable to be used for placeholder variables.
    wcUnusedNames   :: [TH.Name] }

--------------------------------------------------------------------------------
-- The TH expression corresponding to the given code segment. The computation
-- involves three main stages:
--  1. Insert placeholder variables into the literal code at the position of
--     each sub-quote, by folding the code parts into a WriteCode value. The
--     wcLiteralCode field is used to make sure that the generated placeholder
--     names do not occur anywhere else in the code. We take advantage of
--     Haskell's non-strict evaluation by using part of the output of the fold
--     to compute part of its input.
--  2. Parse the generated code using Language.Haskell.Meta.Parse.parseExp
--     provided by the `haskell-src-exts' package, producing a TH expression.
--  3. Substitute each placeholder variable in this TH expression for the
--     expression of the sub-quote it represents, using `subExpM'.
writeCode :: Code W -> TH.Q TH.Exp
writeCode parts
  = case parseExp $ fullCode of
      Right expr -> [| QS.unlines $(subExpM subName expr) |]
      Left msg   -> fail $ msg ++ fullCode
  where
    subName :: TH.Name -> TH.Q (Maybe TH.Exp)
    subName name
      = maybe (return Nothing) (fmap Just) (M.lookup name substitutions)

    WriteCode{
        wcFullCode      = fullCode,
        wcLiteralCode   = literalCode,
        wcSubstitutions = substitutions } = finalState

    finalState = foldr writeCodePart initState $
        [CodeLiteral "["] ++ parts ++ [CodeLiteral "]"]

    initState = WriteCode{
        wcFullCode      = "",
        wcLiteralCode   = "",
        wcSubstitutions = M.empty,
        wcUnusedNames   = names } 

    names = [TH.mkName x | i <- [0..] :: [Integer],
             let x="x"++show i, not $ x `isInfixOf` literalCode]

-- Used in a right fold by writeCode: update a WriteCode state according to the
-- given code segment element.
writeCodePart :: CodePart W -> WriteCode -> WriteCode
writeCodePart (CodeLiteral str) state = state{
    wcFullCode      = str ++ wcFullCode state,
    wcLiteralCode   = str ++ wcLiteralCode state }
writeCodePart (SubQuote w text) state = state{
    wcFullCode      = " "++ TH.pprint name ++" "++ wcFullCode state,
    wcLiteralCode   = " "++ wcLiteralCode state,
    wcSubstitutions = M.insert name (w $ writeTmpl text) (wcSubstitutions state),
    wcUnusedNames   = names }
  where
    name : names = wcUnusedNames state

--------------------------------------------------------------------------------
-- Miscellaneous general utilities.

--------------------------------------------------------------------------------
-- Strip from the second list the longest prefix common to both lists.
stripLongestPrefix :: Eq a => [a] -> [a] -> [a]
stripLongestPrefix (p:ps) (x:xs) | p == x = stripLongestPrefix ps xs
stripLongestPrefix _      xs              = xs

--------------------------------------------------------------------------------
-- Given a structure supporting Generic Programming (i.e. of type Data d => d),
-- replace Template Haskell variable expressions occurring in the structure with
-- arbitrary Template Haskell expressions, when the given function returns Just;
-- or leave them unchanged when it returns Nothing, given the variable's name.
-- The computation is performed inside an arbitrary monad, m.
--
-- Note that the lexical scope of expressions is not respected, and bound
-- variables are replaced as well as free variables; so the caller should make
-- sure that no unintended variables are affected by the operation
subExpM :: (Monad m, Data d) => (TH.Name -> m (Maybe TH.Exp)) -> d -> m d
subExpM sub expr = case cast expr of
    Just (TH.VarE name) -> fmap (fromMaybe expr . (>>= cast)) (sub name)
    _                   -> gmapM (subExpM sub) expr
