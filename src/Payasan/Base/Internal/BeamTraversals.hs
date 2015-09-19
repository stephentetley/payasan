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
  , BeamPitchAlgo(..)
  , transformP

  , BeamDurationAlgo(..)
  , transformD

  ) where



import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a

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
                          , element_trafoP = elemT }) ph = 
    evalRewriteDefault (phraseT ph) st0
  where
    phraseT :: Phrase p1 drn anno -> Mon st (Phrase p2 drn anno)
    phraseT (Phrase bs)         = Phrase <$> mapM barT bs

    barT :: Bar p1 drn anno -> Mon st (Bar p2 drn anno)
    barT (Bar info cs)          = local info $ Bar info <$> mapM noteGroupT cs

    noteGroupT :: NoteGroup p1 drn anno -> Mon st (NoteGroup p2 drn anno)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Beamed cs)      = Beamed <$> mapM noteGroupT cs
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs


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
                             , element_trafoD = elemT }) ph = 
    evalRewriteDefault (phraseT ph) st0
  where
    phraseT :: Phrase pch d1 anno -> Mon st (Phrase pch d2 anno)
    phraseT (Phrase bs)         = Phrase <$> mapM barT bs

    barT :: Bar pch d1 anno -> Mon st (Bar pch d2 anno)
    barT (Bar info cs)          = local info $ Bar info <$> mapM noteGroupT cs

    noteGroupT :: NoteGroup pch d1 anno -> Mon st (NoteGroup pch d2 anno)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Beamed cs)      = Beamed <$> mapM noteGroupT cs
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs


