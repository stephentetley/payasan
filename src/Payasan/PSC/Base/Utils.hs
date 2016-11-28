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
    
    ParsecParser
  , ParsecLexer
  , fullInputParse

  , punctuateSepEnd
  , ppTable

  )  where


import Text.Parsec                              -- package: parsec
import Text.Parsec.Token

import Text.PrettyPrint.HughesPJ                -- package: pretty

import Control.Monad.Identity
import Data.Char (isSpace)


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
    
    
    