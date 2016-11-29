{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Base.Utils
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common utility code for Payasan.PSC
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Base.Utils
  ( 
    
  -- * Parsing
    ParsecParser
  , ParsecLexer
  , fullInputParse

  -- * Pretty printing
  , vsep
  , sepList
  , withString
  , ($?+$)
  , punctuateSepEnd
  , ppTable

    -- * Hughes list
  , H
  , emptyH
  , appendH
  , consH
  , snocH
  , wrapH
  , replicateH
  , toListH
  , fromListH
  
  )  where


import Text.Parsec                              -- package: parsec
import Text.Parsec.Token

import Text.PrettyPrint.HughesPJ                -- package: pretty

import Control.Monad.Identity
import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Parsing

type ParsecParser a        = ParsecT String () Identity a
type ParsecLexer           = GenTokenParser String () Identity




-- | For quasiquoted syntax we want to force full input parse, 
-- otherwise we can get unexpected results (if Parsec finds a 
-- valid prefix it may return that on hitting an error rather 
-- than return the error). 
--
fullInputParse :: forall a. ParsecParser () -> ParsecParser a -> ParsecParser a
fullInputParse white p = white *> parseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


    parseK :: ParsecParser (a, SourcePos, String)
    parseK = (,,) <$> p <*> getPosition <*> getInput

    
    
--------------------------------------------------------------------------------
-- Pretty printing

    
vsep :: [Doc] -> Doc
vsep = sepList ($+$)

sepList :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
sepList _  [] = empty
sepList op (x:xs) = step x xs
  where
    step ac []     = ac
    step ac (d:ds) = step (ac `op` d) ds
    

    

withString :: String -> (String -> Doc) -> Doc
withString ss f = if null ss then empty else f ss




infixl 5 $?+$ 

($?+$) :: Maybe Doc -> Doc -> Doc
($?+$) (Nothing) d2 = d2
($?+$) (Just d1) d2 = d1 $+$ d2

    
-- | Special case pretty printer, probably only useful for 
-- appending bar lines to sections (we generally want to last 
-- bar line to be a terminator). The initial bars are suffixed 
-- with sep, the final bar is suffixed with end.
--
-- Concatentation of bar output is special (e.g. ABC might want 
-- 3-4 per line) so we don't do it here.
--
punctuateSepEnd :: Doc -> Doc -> [Doc] -> [Doc]
punctuateSepEnd sepr end = go 
  where
    go []     = []
    go [d]    = [d <> end]
    go (d:ds) = d <> sepr : go ds


-- | Pretty print a table of n columns.
-- 
-- Horizontally columns are concatenated with the supplied 
-- combinator, vertically rows are printed line-by-line.
--
ppTable :: Int -> (Doc -> Doc -> Doc) -> [Doc] -> Doc
ppTable _    _   []     = empty
ppTable cols hop (x:xs) = vcat $ go 1 x xs
  where
    go n ac (d:ds) | n < cols   = go (n+1) (ac `hop` d) ds
                   | otherwise  = ac : go 1 d ds
    go _ ac []                  = [ac]
    
    
--------------------------------------------------------------------------------
-- Hughes list
-- Should be obsolete, but currently needed by MIDI output...


type H a = [a] -> [a]

emptyH :: H a 
emptyH = id

appendH :: H a -> H a -> H a
appendH f g = f . g

wrapH :: a -> H a 
wrapH a = (a:)

consH :: a -> H a -> H a
consH a f = (a:) . f

snocH :: H a -> a -> H a
snocH f a = f . (a:)

replicateH :: Int -> a -> H a
replicateH i a = fromListH $ replicate i a


toListH :: H a -> [a]
toListH f = f $ []

fromListH :: [a] -> H a
fromListH xs = (xs++)