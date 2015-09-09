{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.BeamPitchTrafo
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversal of Mono syntax for pitch transformation.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.MonoPitchTrafo
  (
    Mon 
  , MonoPitchAlgo(..)
  , transform
  , mapPch
  ) where



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a

data MonoPitchAlgo st pch1 pch2 = MonoPitchAlgo 
    { initial_state     :: st
    , element_trafo     :: forall drn. Element pch1 drn  -> Mon st (Element pch2 drn)
    }


transform :: MonoPitchAlgo st p1 p2 -> Phrase p1 drn -> Phrase p2 drn
transform algo ph = evalRewriteDefault (phraseT algo ph) (initial_state algo)


phraseT :: MonoPitchAlgo st p1 p2 -> Phrase p1 drn -> Mon st (Phrase p2 drn)
phraseT algo (Phrase bs)          = Phrase <$> mapM (barT algo) bs



barT :: MonoPitchAlgo st p1 p2 -> Bar p1 drn -> Mon st (Bar p2 drn)
barT algo (Bar info cs)           = local info $ 
    Bar info <$> mapM (ctxElementT algo) cs

  
ctxElementT :: MonoPitchAlgo st p1 p2 
            -> CtxElement p1 drn 
            -> Mon st (CtxElement p2 drn)
ctxElementT algo (Atom e)         = let elemT = element_trafo algo
                                    in Atom <$> elemT e
ctxElementT algo (Tuplet spec cs) = Tuplet spec <$> mapM (ctxElementT algo) cs



--------------------------------------------------------------------------------
-- Transformation

mapPch :: (pch1 -> pch2) -> Phrase pch1 drn -> Phrase pch2 drn
mapPch fn = transform algo 
  where
    algo  = MonoPitchAlgo { initial_state    = ()
                          , element_trafo    = stepE 
                          }

    stepE (Note p d) = pure $ Note (fn p) d
    stepE (Rest d)   = pure $ Rest d

{-

ctxMapPch :: (KeySig -> pch1 -> pch2) -> Phrase pch1 drn -> Phrase pch2 drn
ctxMapPch fn = transform algo 
  where
    algo  = MonoPitchAlgo { initial_state    = ()
                          , bar_info_action  = \_ -> return ()
                          , element_trafo    = stepE 
                          }

    stepE (Note p d) = (\ks -> Note (fn ks p) d) <$> gets local_key_sig
    stepE (Rest d)   = pure $ Rest d
-}
