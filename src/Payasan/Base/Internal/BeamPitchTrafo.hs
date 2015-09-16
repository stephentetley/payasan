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
import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a

data BeamPitchAlgo st pch1 pch2 = BeamPitchAlgo 
    { initial_state     :: st
    , element_trafo     :: forall drn anno. 
                           Element pch1 drn anno -> Mon st (Element pch2 drn anno)
    }


transform :: BeamPitchAlgo st p1 p2 -> Phrase p1 drn anno -> Phrase p2 drn anno
transform algo ph = evalRewriteDefault (phraseT algo ph) (initial_state algo)


phraseT :: BeamPitchAlgo st p1 p2 -> Phrase p1 drn anno -> Mon st (Phrase p2 drn anno)
phraseT algo (Phrase bs)          = Phrase <$> mapM (barT algo) bs



barT :: BeamPitchAlgo st p1 p2 -> Bar p1 drn anno -> Mon st (Bar p2 drn anno)
barT algo (Bar info cs)           = local info $
     Bar info <$> mapM (noteGroupT algo) cs

  
noteGroupT :: BeamPitchAlgo st p1 p2 
           -> NoteGroup p1 drn anno
           -> Mon st (NoteGroup p2 drn anno)
noteGroupT algo (Atom e)          = let elemT = element_trafo algo
                                    in Atom <$> elemT e
noteGroupT algo (Beamed cs)       = Beamed <$> mapM (noteGroupT algo) cs
noteGroupT algo (Tuplet spec cs)  = Tuplet spec <$> mapM (noteGroupT algo) cs

