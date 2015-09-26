{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Spelling
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pitch spelling for ABC.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.Spelling
  (
    SpellingMap(..) 
  , spellFindNatural
  , spellFindAlteration
  , makeSpellingMap

  ) where


import qualified Payasan.Base.Internal.ABC.Syntax as ABC
import Payasan.Base.Internal.ABC.Syntax (ABCPitch(..))
import Payasan.Base.Internal.ABC.Utils
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Scale
import Payasan.Base.Pitch


import qualified Data.Map as MAP
import qualified Data.Set as SET

--------------------------------------------------------------------------------

-- TODO - now we have a scale object there should be significant 
-- opportunities to simplify this module...


-- | We need to use the pitch label from Pitch as it doesn\'t
-- confuse pitch letter with upper and lower case.



data SpellingMap = SpellingMap 
    { spelling_alterations      :: SET.Set PitchName 
    , spelling_alt_lookups      :: MAP.Map PitchLetter  PitchName
    , spelling_naturals         :: SET.Set PitchLetter
    }
  deriving (Show)



-- | Rendering - initial output translation sets all NATURALS 
-- to NO_ACCIDENTAL, we might have to change them to NATURAL 
-- for printing.
--
spellFindNatural :: SpellingMap -> ABCPitch -> ABCPitch
spellFindNatural (SpellingMap { spelling_alterations = alts 
                              , spelling_naturals    = nats}) 
                 p0@(ABCPitch ac l om) = step ac
  where
    step ABC.NO_ACCIDENTAL      = 
        let (l1,_) = toLetterParts l 
        in if l1 `SET.member` nats then ABCPitch ABC.NATURAL l om  else p0

    step _                      = 
        let (lbl,_,_) = decomposePitch p0 
        in if lbl `SET.member` alts then ABCPitch ABC.NO_ACCIDENTAL l om else p0




-- | Parsing - initial input sets all non-altered notes to 
-- NO_ACCIDENTAL. Non-altered notes in the input can include
-- notes that are actually altered by the key signature. 
-- We need to look up their alteration.
--
spellFindAlteration :: SpellingMap -> ABCPitch -> ABCPitch
spellFindAlteration (SpellingMap { spelling_alt_lookups = finds }) p = 
    case MAP.lookup (pitch_letter name) finds of
        Nothing -> p
        Just name1 -> recomposePitch name1 lc ove
  where
    (name,lc,ove) = decomposePitch p




makeSpellingMap :: Key -> SpellingMap
makeSpellingMap ks = let i = findAlterations ks in buildSpellings i


-- | Make a spelling map with @n@ accidentals. If @n@ is positive
-- the accidentals will be sharps, if @n@ s negative the 
-- accidentals will be flats.
--
buildSpellings :: Int -> SpellingMap
buildSpellings n 
    | abs n > 7 = error "SpellingMap.makeSpellingMap - more sharps/flats than notes."
    | n == 0    = SpellingMap SET.empty MAP.empty SET.empty
    | n >  0    = build $ nsharps n
    | otherwise = build $ nflats (abs n)          
  where
    build lbls = SpellingMap 
                   { spelling_alterations = SET.fromList $ lbls
                   , spelling_alt_lookups = MAP.fromList $ map lookup1 lbls
                   , spelling_naturals    = SET.fromList $ map root lbls
                   }
    
    root (PitchName l _)   = l

    lookup1 :: PitchName -> (PitchLetter, PitchName)
    lookup1 name = (root name, name)


