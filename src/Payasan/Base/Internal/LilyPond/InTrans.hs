{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.InTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Main Syntax, plus /pushing/ render info
-- into bars as this cannot be done during parsing / 
-- quasiquoting.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.InTrans
  (
    translate
  ) where



import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Internal.MainSyntax as T
import Payasan.Base.Internal.Utils

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as T

data St = St { previous_duration   :: Duration 
             , previous_pitch      :: T.Pitch
             }

type Mon a = Trans GlobalRenderInfo St a


previousDuration :: Mon Duration
previousDuration = gets previous_duration

setPrevDuration :: Duration -> Mon ()
setPrevDuration d = puts (\s -> s { previous_duration = d })

previousPitch :: Mon (Maybe T.Pitch)
previousPitch = fn <$> asks global_pitch_directive <*> gets previous_pitch
  where
    fn (AbsPitch) _    = Nothing
    fn (RelPitch {}) p = Just p

setPrevPitch :: T.Pitch -> Mon ()
setPrevPitch p = puts (\s -> s { previous_pitch = p })

translate :: GlobalRenderInfo -> LyPhrase -> T.Phrase T.Pitch Duration
translate info ph = evalTrans (phraseT ph) info state_zero
  where
    -- The first duration should never match then we always start
    -- printing
    state_zero = St { previous_duration = dQuarter
                    , previous_pitch    = pitch_zero
                    }
    -- If AbsPitch then /previous pitch/ will never be used
    pitch_zero = case global_pitch_directive info of
                    RelPitch pch -> pch
                    AbsPitch -> T.middle_c      


phraseT :: LyPhrase -> Mon (T.Phrase T.Pitch Duration)
phraseT (Phrase bs)          = T.Phrase <$> mapM barT bs



barT :: LyBar  -> Mon (T.Bar T.Pitch Duration)
barT (Bar info cs)              = 
    do { css <- mapM ctxElementT cs
       ; return $ T.Bar info (concat css)
       }


-- | Remember - a beamed CtxElement may generate 1+ elements
--
ctxElementT :: LyCtxElement -> Mon [T.CtxElement T.Pitch Duration]
ctxElementT (Atom e)            = (wrapL . T.Atom) <$> elementT e

ctxElementT (Tuplet spec cs)    = 
    (wrapL . T.Tuplet spec . concat) <$> mapM ctxElementT cs

ctxElementT (Beamed cs)         = concat <$> mapM ctxElementT cs



elementT :: LyElement  -> Mon (T.Element T.Pitch Duration)
elementT (NoteElem a)           = T.NoteElem <$> noteT a
elementT (Rest d)               = T.Rest   <$> durationT d
elementT (Chord ps d)           = T.Chord  <$> mapM pitchT ps <*> durationT d
elementT (Graces ns)            = T.Graces <$> mapM noteT ns


noteT :: LyNote -> Mon (T.Note T.Pitch Duration)
noteT (Note pch drn)          = T.Note <$> pitchT pch <*> durationT drn


-- No previous pitch indicates Absolute pitch mode
pitchT :: Pitch -> Mon T.Pitch
pitchT p1 = previousPitch >>= step 
  where
    step (Nothing)   = pure $ toPitchAbs p1
    step (Just p0)   = let tp1 = toPitchRel p1 p0 in setPrevPitch tp1 >> return tp1
                              




durationT :: NoteLength -> Mon Duration
durationT (DrnDefault)    = previousDuration
durationT (DrnExplicit d) = setPrevDuration d >> return d


--------------------------------------------------------------------------------
-- Helpers

wrapL :: a -> [a]
wrapL a = [a]




