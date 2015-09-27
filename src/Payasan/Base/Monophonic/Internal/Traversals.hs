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
  , collectP
  , mapPitch
  , ctxMapPitch   -- TEMP ?
  , foldPitch 


  , MonoDurationAlgo(..)
  , transformD
  , collectD
  , mapDuration
  , foldDuration

  , MonoAnnoAlgo(..)
  , transformA
  , collectA
  , mapAnno
  , foldAnno

  , MonoPitchAnnoAlgo(..)
  , transformPA
  , collectPA
  , mapPitchAnno
  , foldPitchAnno

  ) where



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Data.Foldable (foldlM)

type Mon st a = Rewrite st a


-- | Do not expose this as it is too general / complex.
--
genCollect :: forall st pch drn anno ac.
              (ac -> Element pch drn anno -> Mon st ac) 
           -> ac 
           -> st
           -> Phrase pch drn anno 
           -> ac
genCollect mf a0 st ph = evalRewriteDefault (phraseC a0 ph) st
  where

    phraseC :: ac -> Phrase pch drn anno -> Mon st ac
    phraseC ac (Phrase info bs) = local info (foldlM barC ac bs)

    barC :: ac -> Bar pch drn anno -> Mon st ac
    barC ac (Bar  cs)           = foldlM noteGroupC ac cs

    noteGroupC :: ac -> NoteGroup pch drn anno -> Mon st ac
    noteGroupC ac (Atom e)      = mf ac e
    noteGroupC ac (Tuplet _ cs) = foldlM noteGroupC ac cs


-- | Do not expose this as it is too general / complex.
--
genTransform :: forall st p1 p2 d1 d2 a1 a2. 
                (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
             -> st
             -> Phrase p1 d1 a1
             -> Phrase p2 d2 a2
genTransform elemT st0 ph = 
    evalRewriteDefault (phraseT ph) st0
  where

    phraseT :: Phrase p1 d1 a1 -> Mon st (Phrase p2 d2 a2) 
    phraseT (Phrase info bs)    = local info (Phrase info <$> mapM barT bs)

    barT :: Bar p1 d1 a1 -> Mon st (Bar p2 d2 a2)
    barT (Bar cs)               = Bar <$> mapM noteGroupT cs

    noteGroupT :: NoteGroup p1 d1 a1 -> Mon st (NoteGroup p2 d2 a2)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Tuplet spec cs) = Tuplet spec <$> mapM noteGroupT cs



--
-- Design note - this leaks /shape/, possible to change a rest 
-- to a note or vice-versa.
-- 
-- However, this functionality is for /library writers/ not
-- /top level users/ where it seems that acknowledging the 
-- note-rest distinction is useful.
--


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
                          , element_trafoP = elemT }) = genTransform elemT st0


-- | This is a seems less generally useful than @transformP@ 
-- so we don\'t expose an /algo/.
-- 
collectP :: forall st pch drn anno ac.
            (ac -> pch -> Mon st ac) 
         -> ac 
         -> st
         -> Phrase pch drn anno 
         -> ac
collectP mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note p _ _)    = mf ac p
    elementC ac (Rest {})       = pure $ ac



--------------------------------------------------------------------------------
-- Transformation

mapPitch :: (pch1 -> pch2) -> Phrase pch1 drn anno -> Phrase pch2 drn anno
mapPitch fn = ctxMapPitch (\_ p -> fn p)


ctxMapPitch :: (Key -> pch1 -> pch2) 
            -> Phrase pch1 drn anno 
            -> Phrase pch2 drn anno
ctxMapPitch fn = transformP algo 
  where
    algo  = MonoPitchAlgo { initial_stateP    = ()
                          , element_trafoP    = stepE 
                          }

    stepE (Note p d a)  = (\ks -> Note (fn ks p) d a) <$> asksLocal local_key
    stepE (Rest d)      = pure $ Rest d


foldPitch :: (ac -> pch -> ac) -> ac -> Phrase pch drn anno -> ac
foldPitch fn a0 ph = collectP step a0 () ph
  where
    step ac p   = pure $ fn ac p

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
                             , element_trafoD = elemT }) = genTransform elemT st0


-- | This is a seems less generally useful than @transformD@ 
-- so we don\'t expose an /algo/.
-- 
collectD :: forall st pch drn anno ac.
            (ac -> drn -> Mon st ac) 
         -> ac 
         -> st
         -> Phrase pch drn anno 
         -> ac
collectD mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note _ d _)    = mf ac d
    elementC ac (Rest {})       = pure $ ac

--------------------------------------------------------------------------------
-- Transformation

-- Note - increasing or decreasing duration would imply 
-- recalculating bar lines.

mapDuration :: (drn1 -> drn2) -> Phrase pch drn1 anno -> Phrase pch drn2 anno
mapDuration fn = transformD algo 
  where
    algo  = MonoDurationAlgo { initial_stateD   = ()
                             , element_trafoD   = stepE 
                             }

    stepE (Note p d a)  = pure $ Note p (fn d) a
    stepE (Rest d)      = pure $ Rest (fn d)


foldDuration :: (ac -> drn -> ac) -> ac -> Phrase pch drn anno -> ac
foldDuration fn a0 ph = collectD step a0 () ph
  where
    step ac d   = pure $ fn ac d

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
                         , element_trafoA = elemT }) = genTransform elemT st0


collectA :: forall st pch drn anno ac.
            (ac -> anno -> Mon st ac) 
         -> ac 
         -> st
         -> Phrase pch drn anno 
         -> ac
collectA mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note _ _ a)    = mf ac a
    elementC ac (Rest {})       = pure $ ac


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


foldAnno :: (ac -> anno -> ac) -> ac -> Phrase pch drn anno -> ac
foldAnno fn a0 ph = collectA step a0 () ph
  where
    step ac a   = pure $ fn ac a

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
                               , element_trafoPA = elemT }) = 
    genTransform elemT st0


collectPA :: forall st pch drn anno ac.
             (ac -> pch -> anno -> Mon st ac) 
          -> ac 
          -> st
          -> Phrase pch drn anno 
          -> ac
collectPA mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note p _ a)    = mf ac p a
    elementC ac (Rest {})       = pure $ ac



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

foldPitchAnno :: (ac -> pch -> anno -> ac) -> ac -> Phrase pch drn anno -> ac
foldPitchAnno fn a0 ph = collectPA step a0 () ph
  where
    step ac p a   = pure $ fn ac p a
