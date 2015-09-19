{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.Traversals
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals of Mono syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Traversals
  (
    Mon 
  , MonoPitchAlgo(..)
  , transformP
  , mapPch
  , ctxMapPch   -- TEMP ?

  , MonoDurationAlgo(..)
  , transformD
  , mapDrn

  , MonoAnnoAlgo(..)
  , transformA
  , mapAnno

  , MonoPitchAnnoAlgo(..)
  , transformPA
  , mapPitchAnno

  ) where



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a

data MonoPitchAlgo st pch1 pch2 = MonoPitchAlgo 
    { initial_stateP  :: st
    , element_trafoP  :: forall drn anno. 
                         Element pch1 drn anno -> Mon st (Element pch2 drn anno)
    }


transformP :: forall st p1 p2 drn anno. 
              MonoPitchAlgo st p1 p2 
           -> Phrase p1 drn anno 
           -> Phrase p2 drn anno
transformP (MonoPitchAlgo { initial_stateP = st0 
                          , element_trafoP = elemT }) ph = 
    evalRewriteDefault (phraseT ph) st0
  where

    phraseT :: Phrase p1 drn anno -> Mon st (Phrase p2 drn anno) 
    phraseT (Phrase bs)         = Phrase <$> mapM barT bs

    barT :: Bar p1 drn anno -> Mon st (Bar p2 drn anno)
    barT (Bar info cs)          = local info $ Bar info <$> mapM noteGroupT cs

    noteGroupT :: NoteGroup p1 drn anno -> Mon st (NoteGroup p2 drn anno)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs



--------------------------------------------------------------------------------
-- Transformation

mapPch :: (pch1 -> pch2) -> Phrase pch1 drn anno -> Phrase pch2 drn anno
mapPch fn = ctxMapPch (\_ p -> fn p)


ctxMapPch :: (Key -> pch1 -> pch2) 
          -> Phrase pch1 drn anno 
          -> Phrase pch2 drn anno
ctxMapPch fn = transformP algo 
  where
    algo  = MonoPitchAlgo { initial_stateP    = ()
                          , element_trafoP    = stepE 
                          }

    stepE (Note p d a)  = (\ks -> Note (fn ks p) d a) <$> asksLocal local_key
    stepE (Rest d)      = pure $ Rest d

--------------------------------------------------------------------------------
-- Duration

data MonoDurationAlgo st drn1 drn2 = MonoDurationAlgo 
    { initial_stateD :: st
    , element_trafoD :: forall pch anno. 
                        Element pch drn1 anno -> Mon st (Element pch drn2 anno)
    }


transformD :: forall st pch d1 d2 anno.
              MonoDurationAlgo st d1 d2 
           -> Phrase pch d1 anno 
           -> Phrase pch d2 anno
transformD (MonoDurationAlgo { initial_stateD = st0 
                             , element_trafoD = elemT }) ph = 
    evalRewriteDefault (phraseT ph) st0
  where
    phraseT :: Phrase pch d1 anno -> Mon st (Phrase pch d2 anno)
    phraseT (Phrase bs)         = Phrase <$> mapM barT bs

    barT :: Bar pch d1 anno -> Mon st (Bar pch d2 anno)
    barT (Bar info cs)          = local info $ Bar info <$> mapM noteGroupT cs
  
    noteGroupT :: NoteGroup pch d1 anno -> Mon st (NoteGroup pch d2 anno)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs


--------------------------------------------------------------------------------
-- Transformation

-- Note - increasing or decreasing duration would imply 
-- recalculating bar lines.

mapDrn :: (drn1 -> drn2) -> Phrase pch drn1 anno -> Phrase pch drn2 anno
mapDrn fn = transformD algo 
  where
    algo  = MonoDurationAlgo { initial_stateD   = ()
                             , element_trafoD   = stepE 
                             }

    stepE (Note p d a)  = pure $ Note p (fn d) a
    stepE (Rest d)      = pure $ Rest (fn d)

--------------------------------------------------------------------------------
-- Annotation


data MonoAnnoAlgo st anno1 anno2 = MonoAnnoAlgo 
    { initial_stateA  :: st
    , element_trafoA  :: forall pch drn. 
                         Element pch drn anno1 -> Mon st (Element pch drn anno2)
    }


transformA :: forall st pch drn a1 a2.
              MonoAnnoAlgo st a1 a2
           -> Phrase pch drn a1 
           -> Phrase pch drn a2
transformA (MonoAnnoAlgo { initial_stateA = st0 
                         , element_trafoA = elemT }) ph = 
    evalRewriteDefault (phraseT ph) st0
  where
    phraseT :: Phrase pch drn a1 -> Mon st (Phrase pch drn a2) 
    phraseT (Phrase bs)         = Phrase <$> mapM barT bs

    barT :: Bar pch drn a1 -> Mon st (Bar pch drn a2)
    barT (Bar info cs)          = local info $ Bar info <$> mapM noteGroupT cs

    noteGroupT :: NoteGroup pch drn a1 -> Mon st (NoteGroup pch drn a2)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs



--------------------------------------------------------------------------------
-- Transformation


mapAnno :: (anno1 -> anno2) -> Phrase pch drn anno1 -> Phrase pch drn anno2
mapAnno fn = transformA algo 
  where
    algo  = MonoAnnoAlgo { initial_stateA   = ()
                         , element_trafoA   = stepE 
                         }

    stepE (Note p d a)  = pure $ Note p d (fn a)
    stepE (Rest d)      = pure $ Rest d



--------------------------------------------------------------------------------
-- Pitch and Annotation

data MonoPitchAnnoAlgo st pch1 anno1 pch2 anno2 = MonoPitchAnnoAlgo 
    { initial_statePA :: st
    , element_trafoPA :: 
             forall drn. 
             Element pch1 drn anno1 -> Mon st (Element pch2 drn anno2)
    }


transformPA :: forall st p1 p2 drn a1 a2.
               MonoPitchAnnoAlgo st p1 a1 p2 a2
            -> Phrase p1 drn a1 
            -> Phrase p2 drn a2
transformPA (MonoPitchAnnoAlgo { initial_statePA = st0 
                               , element_trafoPA = elemT }) ph = 
    evalRewriteDefault (phraseT ph) st0
  where
    phraseT :: Phrase p1 drn a1 -> Mon st (Phrase p2 drn a2) 
    phraseT (Phrase bs)         = Phrase <$> mapM barT bs

    barT :: Bar p1 drn a1 -> Mon st (Bar p2 drn a2)
    barT (Bar info cs)          = local info $ Bar info <$> mapM noteGroupT cs

  
    noteGroupT :: NoteGroup p1 drn a1 -> Mon st (NoteGroup p2 drn a2)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs



--------------------------------------------------------------------------------
-- Transformation


mapPitchAnno :: (p1 -> a1 -> (p2,a2)) -> Phrase p1 drn a1 -> Phrase p2 drn a2
mapPitchAnno fn = transformPA algo 
  where
    algo  = MonoPitchAnnoAlgo { initial_statePA   = ()
                              , element_trafoPA   = stepE 
                              }

    stepE (Note p d a)  = let (p1,a1) = fn p a in pure $ Note p1 d a1
    stepE (Rest d)      = pure $ Rest d