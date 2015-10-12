{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.BeamTraversals
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals for Beam syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.BeamTraversals
  (
    Mon 
  , liftElementTrafo

  , BeamPitchAlgo(..)
  , transformP

  , BeamDurationAlgo(..)
  , transformD

  , BeamPitchAnnoAlgo(..)
  , transformPA

  ) where



import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a


genTransform :: forall st p1 p2 d1 d2 a1 a2. 
                (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
             -> st
             -> Phrase p1 d1 a1
             -> Phrase p2 d2 a2
genTransform elemT st0 ph = 
    evalRewriteDefault (phraseT ph) st0
  where
    phraseT :: Phrase p1 d1 a1 -> Mon st (Phrase p2 d2 a2)
    phraseT (Phrase bs)         = Phrase <$> mapM barT bs

    barT :: Bar p1 d1 a1 -> Mon st (Bar p2 d2 a2)
    barT (Bar info cs)          = local info $ Bar info <$> mapM noteGroupT cs

    noteGroupT :: NoteGroup p1 d1 a1 -> Mon st (NoteGroup p2 d2 a2)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Beamed cs)      = Beamed <$> mapM noteGroupT cs
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs


--------------------------------------------------------------------------------
-- Lift a pure Element transformer

liftElementTrafo :: (Element p1 d1 a1 -> Element p2 d2 a2) 
                 -> Element p1 d1 a1 
                 -> Mon () (Element p2 d2 a2)
liftElementTrafo f = \e -> return (f e)

--------------------------------------------------------------------------------
-- Duration

data BeamPitchAlgo st pch1 pch2 = BeamPitchAlgo 
    { initial_stateP :: st
    , element_trafoP :: forall drn anno. 
                        Element pch1 drn anno -> Mon st (Element pch2 drn anno)
    }


transformP :: forall st p1 p2 drn anno.
              BeamPitchAlgo st p1 p2 
           -> Phrase p1 drn anno 
           -> Phrase p2 drn anno
transformP (BeamPitchAlgo { initial_stateP = st0 
                          , element_trafoP = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Duration

data BeamDurationAlgo st drn1 drn2 = BeamDurationAlgo 
    { initial_stateD :: st
    , element_trafoD :: forall pch anno. 
                        Element pch drn1 anno -> Mon st (Element pch drn2 anno)
    }


transformD :: forall st pch d1 d2 anno.
              BeamDurationAlgo st d1 d2 
           -> Phrase pch d1 anno 
           -> Phrase pch d2 anno
transformD (BeamDurationAlgo { initial_stateD = st0 
                             , element_trafoD = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Duration

data BeamPitchAnnoAlgo st pch1 anno1 pch2 anno2 = BeamPitchAnnoAlgo 
    { initial_statePA :: st
    , element_trafoPA :: 
             forall drn. 
             Element pch1 drn anno1 -> Mon st (Element pch2 drn anno2)
    }


transformPA :: forall st p1 p2 drn a1 a2.
               BeamPitchAnnoAlgo st p1 a1 p2 a2
            -> Phrase p1 drn a1 
            -> Phrase p2 drn a2
transformPA (BeamPitchAnnoAlgo { initial_statePA = st0 
                               , element_trafoPA = elemT }) = 
    genTransform elemT st0
