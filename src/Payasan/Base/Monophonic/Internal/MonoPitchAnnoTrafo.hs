{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.MonoPitchAnnoTrafo
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversal of Mono syntax for annotation transformation.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.MonoPitchAnnoTrafo
  (
    Mon 
  , MonoPitchAnnoAlgo(..)
  , transform
  , mapPitchAnno
  ) where



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a

data MonoPitchAnnoAlgo st pch1 anno1 pch2 anno2 = MonoPitchAnnoAlgo 
    { initial_state :: st
    , element_trafo :: forall drn. 
                       Element pch1 drn anno1 -> Mon st (Element pch2 drn anno2)
    }


transform :: MonoPitchAnnoAlgo st p1 a1 p2 a2
          -> Phrase p1 drn a1 
          -> Phrase p2 drn a2
transform algo ph = evalRewriteDefault (phraseT algo ph) (initial_state algo)

phraseT :: MonoPitchAnnoAlgo st p1 a1 p2 a2 
        -> Phrase p1 drn a1 
        -> Mon st (Phrase p2 drn a2) 
phraseT algo (Phrase bs)          = Phrase <$> mapM (barT algo) bs



barT :: MonoPitchAnnoAlgo st p1 a1 p2 a2 
     -> Bar p1 drn a1 
     -> Mon st (Bar p2 drn a2)
barT algo (Bar info cs)           = local info $ 
    Bar info <$> mapM (noteGroupT algo) cs

  
noteGroupT :: MonoPitchAnnoAlgo st p1 a1 p2 a2 
           -> NoteGroup p1 drn a1
           -> Mon st (NoteGroup p2 drn a2)
noteGroupT algo (Atom e)          = let elemT = element_trafo algo
                                    in Atom <$> elemT e
noteGroupT algo (Tuplet spec cs)  = Tuplet spec <$> mapM (noteGroupT algo) cs



--------------------------------------------------------------------------------
-- Transformation


mapPitchAnno :: (p1 -> a1 -> (p2,a2)) -> Phrase p1 drn a1 -> Phrase p2 drn a2
mapPitchAnno fn = transform algo 
  where
    algo  = MonoPitchAnnoAlgo { initial_state    = ()
                              , element_trafo    = stepE 
                              }

    stepE (Note p d a)  = let (p1,a1) = fn p a in pure $ Note p1 d a1
    stepE (Rest d)      = pure $ Rest d

