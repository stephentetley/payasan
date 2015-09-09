{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.MonoDurationTrafo
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversal of Mono syntax for duration transformation.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.MonoDurationTrafo
  (
    Mon 
  , MonoDurationAlgo(..)
  , transform
  , mapDrn
  ) where


import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a

data MonoDurationAlgo st drn1 drn2 = MonoDurationAlgo 
    { initial_state     :: st
    , element_trafo     :: forall pch. Element pch drn1  -> Mon st (Element pch drn2)
    }


transform :: MonoDurationAlgo st d1 d2 -> Phrase pch d1 -> Phrase pch d2
transform algo ph = evalRewriteDefault (phraseT algo ph) (initial_state algo)


phraseT :: MonoDurationAlgo st d1 d2 -> Phrase pch d1 -> Mon st (Phrase pch d2)
phraseT algo (Phrase bs)          = Phrase <$> mapM (barT algo) bs



barT :: MonoDurationAlgo st d1 d2 -> Bar pch d1 -> Mon st (Bar pch d2)
barT algo (Bar info cs)           = local info $ 
    Bar info <$> mapM (ctxElementT algo) cs


  
ctxElementT :: MonoDurationAlgo st d1 d2 
            -> CtxElement pch d1
            -> Mon st (CtxElement pch d2)
ctxElementT algo (Atom e)         = let elemT = element_trafo algo
                                    in Atom <$> elemT e
ctxElementT algo (Tuplet spec cs) = Tuplet spec <$> mapM (ctxElementT algo) cs


--------------------------------------------------------------------------------
-- Transformation

-- Note - increasing or decreasing duration would imply 
-- recalculating bar lines.

mapDrn :: (drn1 -> drn2) -> Phrase pch drn1 -> Phrase pch drn2
mapDrn fn = transform algo 
  where
    algo  = MonoDurationAlgo { initial_state    = ()
                             , element_trafo    = stepE 
                             }

    stepE (Note p d) = pure $ Note p (fn d)
    stepE (Rest d)   = pure $ Rest (fn d)
