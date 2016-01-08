{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}

module Indent(indent) where

import qualified Data.Map.Strict           as M
import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified QuoteString               as QS

import Data.Maybe                  (fromMaybe)
import Data.List                   (isInfixOf, stripPrefix)
import Data.Monoid                 ((<>))
import Data.String                 (fromString)
import Data.Data                   (Data(gmapM))
import Data.Typeable               (cast)
import Control.Monad               (MonadPlus(mzero), guard)
import Control.Monad.Trans         (MonadTrans(lift))
import Control.Monad.Trans.Maybe   (MaybeT(runMaybeT))
import Language.Haskell.Meta.Parse (parseExp)

--------------------------------------------------------------------------------
data Quote a
  = Quote a [TextPart a]
  deriving (Show, Functor)

data TextPart a
  = TextString a String | TextCode a [CodePart a]
  deriving (Show, Functor)

data CodePart a
  = CodeString String | CodeText a [TextPart a]
  deriving (Show, Functor)

type W = TH.ExpQ -> TH.ExpQ

--------------------------------------------------------------------------------
indent :: TH.QuasiQuoter
indent = TH.QuasiQuoter{
    TH.quoteExp  = quoteExp,
    TH.quotePat  = quoteFail "indent" "pattern",
    TH.quoteType = quoteFail "indent" "type",
    TH.quoteDec  = quoteFail "indent" "declaration" }

quoteExp :: String -> TH.ExpQ
quoteExp = writeQuote . indentQuote . parseQuote

quoteFail :: String -> String -> String -> TH.Q a
quoteFail name context _
  = fail $ "Error: ["++name++"|...|] cannot be used in a "++context++" context."

--------------------------------------------------------------------------------
writeQuote :: Quote W -> TH.ExpQ
writeQuote (Quote w parts) = w (writeText parts)

writeText :: [TextPart W] -> TH.ExpQ
writeText _parts = do
    _parts <- mapM writeTextPart _parts
    case _parts of
        []      -> [| "" |]
        [p]     -> [| $(return p) |]
        [p, p'] -> [| $(return p) <> $(return p') |]
        _parts  -> [| mconcat $(return $ TH.ListE _parts) |]

writeTextPart :: TextPart W -> TH.ExpQ
writeTextPart (TextString w str)  = w [| fromString str |]
writeTextPart (TextCode   w code) = w (writeCode code)

data WriteCode = WriteCode{
    wcFullCode      :: String,
    wcLiteralCode   :: String,
    wcSubstitutions :: M.Map TH.Name TH.ExpQ,
    wcUnusedNames   :: [TH.Name] }

writeCode :: [CodePart W] -> TH.ExpQ
writeCode parts
  = case parseExp $ fullCode of
      Right expr ->
        [| QS.unlines $(subExpQ substitutions expr) |]
      Left msg -> fail $ msg ++ fullCode
  where
    parts' = [CodeString "["] ++ parts ++ [CodeString "]"]
    WriteCode{
        wcFullCode      = fullCode,
        wcLiteralCode   = literalCode,
        wcSubstitutions = substitutions
    } = foldr writeCodePart initState parts'
    initState = WriteCode{
        wcFullCode      = "",
        wcLiteralCode   = "",
        wcSubstitutions = M.empty,
        wcUnusedNames   = names }
    names = do
        i <- [0..] :: [Integer]
        let x = "x" ++ show i
        guard . not $ x `isInfixOf` literalCode
        return $ TH.mkName x

writeCodePart :: CodePart W -> WriteCode -> WriteCode
writeCodePart (CodeString str) state = state{
    wcFullCode      = str ++ wcFullCode state,
    wcLiteralCode   = str ++ wcLiteralCode state }
writeCodePart (CodeText w text) state = state{
    wcFullCode      = " "++ TH.pprint name ++" "++ wcFullCode state,
    wcLiteralCode   = " "++ wcLiteralCode state,
    wcSubstitutions = M.insert name (w $ writeText text) (wcSubstitutions state),
    wcUnusedNames   = names }
  where
    name : names = wcUnusedNames state

--------------------------------------------------------------------------------
type Indent = String
type Dedent = String

indentQuote :: Quote () -> Quote W
indentQuote (Quote _ parts)
  = Quote id (indentText parts)

indentText :: [TextPart ()] -> [TextPart W]
indentText (TextString _ str : parts)
  = indentText' "" ded $ TextString () str' : parts
  where
    str' = fromMaybe str (stripPrefix "\n" str)
    ded  = takeWhile (`elem` [' ','\t']) str'
indentText parts
  = indentText' "" "" parts

indentText' :: Indent -> Dedent -> [TextPart ()] -> [TextPart W]
indentText' ind ded (TextString _ str : parts)
  = TextString id (QS.unlines ls) : indentText' ind' ded parts
  where
    ls = map (stripLongestPrefix ded) (QS.lines str)
    ind' = map (\c -> if c=='\t' then '\t' else ' ') $ case reverse ls of
        l:_:_ -> l
        [l]   -> ind ++ l
        []    -> error "impossible"
indentText' ind ded (TextCode _ code : parts)
  = TextCode (indentCodeExpr ind) (indentCode code) : indentText' ind ded parts
indentText' _ _ []
  = []

indentCode :: [CodePart ()] -> [CodePart W]
indentCode = map $ \part -> case part of
    CodeText _ text -> CodeText id (indentText text)
    CodeString str  -> CodeString str

indentCodeExpr :: Indent -> TH.ExpQ -> TH.ExpQ
indentCodeExpr ind expr = [| indentCodeString ind $(expr) |]

indentCodeString :: QS.QuoteString s => Indent -> s -> s
indentCodeString ind str
  = QS.unlines $ take 1 ls ++ map (fromString ind <>) (drop 1 ls)
  where
    ls = QS.lines . fromMaybe str $ QS.stripSuffix "\n" str

--------------------------------------------------------------------------------
parseQuote :: String -> Quote ()
parseQuote  = Quote () . pQ "" where
    pQ acc (':':'[':str) = flush acc ++ TextCode () code : pQ "" str'
                         where (code, str') = parseCode str
    pQ acc ('~':c:str)   = pQ (c:acc) str
    pQ acc (c:str)       = pQ (c:acc) str
    pQ acc ""            = flush acc

    flush ""  = []
    flush acc = [TextString () $ reverse acc]

parseCode :: String -> ([CodePart ()], String)
parseCode = pC "" where
    pC acc ('[':':':str) = (flush acc ++ CodeText () text : parts, str'')
                         where (text, str')   = parseText str
                               (parts, str'') = parseCode str'
    pC acc (']':':':str) = (flush acc, str)
    pC acc ('~':c:str)   = pC (c:acc) str
    pC acc (c:str)       = pC (c:acc) str
    pC _ ""              = error "Parse error: unexpected end of string."

    flush ""  = []
    flush acc = [CodeString $ reverse acc]

parseText :: String -> ([TextPart ()], String)
parseText = pT "" where
    pT acc (':':'[':str) = (flush acc ++ TextCode () code : parts, str'')
                         where (code, str')   = parseCode str
                               (parts, str'') = parseText str'
    pT acc (':':']':str) = (flush acc, str)
    pT acc ('~':c:str)   = pT (c:acc) str
    pT acc (c:str)       = pT (c:acc) str
    pT _ ""              = error "Parse error: unexpected end of string."

    flush ""  = []
    flush acc = [TextString () $ reverse acc]

--------------------------------------------------------------------------------
subExpQ :: Data d => M.Map TH.Name TH.ExpQ -> d -> TH.Q d
subExpQ subs expr = do
    mExpr' <- runMaybeT $ do
        TH.VarE name <- maybe mzero return $ cast expr
        qExpr' <- maybe mzero return $ M.lookup name subs
        expr' <- lift qExpr'
        maybe mzero return $ cast expr'
    maybe descend return mExpr'
  where
    descend = gmapM (subExpQ subs) expr

stripLongestPrefix :: Eq a => [a] -> [a] -> [a]
stripLongestPrefix (p:ps) (x:xs) | p == x    = stripLongestPrefix ps xs
stripLongestPrefix _      xs     | otherwise = xs

