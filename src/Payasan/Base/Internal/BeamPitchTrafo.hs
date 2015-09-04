{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.BeamPitchTrafo
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversal of Beam syntax for pitch transformation.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.BeamPitchTrafo
  (
    Mon 
  , BeamPitchAlgo(..)
  , transform
  ) where



import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.Utils


type Mon st a = Trans () st a

data BeamPitchAlgo st pch1 pch2 = BeamPitchAlgo 
    { initial_state     :: st
    , bar_info_action   :: LocalRenderInfo -> Mon st ()
    , element_trafo     :: forall drn. Element pch1 drn  -> Mon st (Element pch2 drn)
    }


transform :: BeamPitchAlgo st p1 p2 -> Phrase p1 drn -> Phrase p2 drn
transform algo ph = evalTrans (phraseT algo ph) () (initial_state algo)


phraseT :: BeamPitchAlgo st p1 p2 -> Phrase p1 drn -> Mon st (Phrase p2 drn)
phraseT algo (Phrase bs)          = Phrase <$> mapM (barT algo) bs



barT :: BeamPitchAlgo st p1 p2 -> Bar p1 drn -> Mon st (Bar p2 drn)
barT algo (Bar info cs)           = 
    do { barInfo info
       ; cs1 <- mapM (ctxElementT algo) cs
       ; return $ Bar info cs1 
       }
  where
    barInfo = bar_info_action algo
  
ctxElementT :: BeamPitchAlgo st p1 p2 
            -> CtxElement p1 drn 
            -> Mon st (CtxElement p2 drn)
ctxElementT algo (Atom e)         = let elemT = element_trafo algo
                                    in Atom <$> elemT e
ctxElementT algo (Beamed cs)      = Beamed <$> mapM (ctxElementT algo) cs
ctxElementT algo (Tuplet spec cs) = Tuplet spec <$> mapM (ctxElementT algo) cs


{-

elementT :: (drn -> Duration) -> Element pch drn -> T.Element pch Duration
elementT fn (NoteElem a)        = T.NoteElem $ noteT fn a
elementT fn (Rest d)            = T.Rest $ fn d
elementT fn (Chord ps d)        = T.Chord ps $ fn d
elementT fn (Graces ns)         = T.Graces $ map (noteT fn) ns


noteT :: (drn -> Duration) -> Note pch drn -> T.Note pch Duration
noteT fn (Note pch drn) = T.Note pch (fn drn)

-}