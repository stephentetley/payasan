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
  ) where



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.Utils ( Trans, evalTrans )


type Mon st a = Trans () st a

data MonoPitchAlgo st pch1 pch2 = MonoPitchAlgo 
    { initial_state     :: st
    , bar_info_action   :: LocalRenderInfo -> Mon st ()
    , element_trafo     :: forall drn. Element pch1 drn  -> Mon st (Element pch2 drn)
    }


transform :: MonoPitchAlgo st p1 p2 -> Phrase p1 drn -> Phrase p2 drn
transform algo ph = evalTrans (phraseT algo ph) () (initial_state algo)


phraseT :: MonoPitchAlgo st p1 p2 -> Phrase p1 drn -> Mon st (Phrase p2 drn)
phraseT algo (Phrase bs)          = Phrase <$> mapM (barT algo) bs



barT :: MonoPitchAlgo st p1 p2 -> Bar p1 drn -> Mon st (Bar p2 drn)
barT algo (Bar info cs)           = 
    do { barInfo info
       ; cs1 <- mapM (ctxElementT algo) cs
       ; return $ Bar info cs1 
       }
  where
    barInfo = bar_info_action algo
  
ctxElementT :: MonoPitchAlgo st p1 p2 
            -> CtxElement p1 drn 
            -> Mon st (CtxElement p2 drn)
ctxElementT algo (Atom e)         = let elemT = element_trafo algo
                                    in Atom <$> elemT e
ctxElementT algo (Tuplet spec cs) = Tuplet spec <$> mapM (ctxElementT algo) cs

