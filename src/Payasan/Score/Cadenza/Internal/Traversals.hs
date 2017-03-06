{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.Traversals
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals of Cadenza syntax.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.Traversals
  (
    Mon 

  , CadenzaAlgo(..)
  , transformCadenza

  , collectP
  , mapPitch
  , ctxMapPitch   -- TEMP ?
  , foldPitch 


  , collectD
  , mapDuration
  , foldDuration

  , collectA
  , mapAnno
  , foldAnno

  , collectPA
  , mapPitchAnno
  , foldPitchAnno

  , censorPunctuation
  , censorAnno
  , skipToRest

  ) where


import Payasan.Score.Cadenza.Internal.Syntax

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Scale

import Control.Monad.Identity           -- package: mtl
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable (foldlM)
import Data.Maybe



type Mon st = ReaderT SectionInfo (StateT st Identity)

evalRewrite :: Mon st a -> SectionInfo -> st -> a
evalRewrite mf r s = runIdentity (evalStateT (runReaderT mf r) s)


-- | Do not expose this as it is too general / complex.
--
genCollect :: forall st pch drn anno ac.
              (ac -> Element pch drn anno -> Mon st ac) 
           -> ac 
           -> st
           -> Section pch drn anno 
           -> ac
genCollect mf a0 st ph = 
    evalRewrite (partC a0 ph) default_section_info st
  where

    partC :: ac -> Section pch drn anno -> Mon st ac
    partC ac (Section _ info bs)     = local (const info) (foldlM noteGroupC ac bs)

    noteGroupC :: ac -> NoteGroup pch drn anno -> Mon st ac
    noteGroupC ac (Atom e)      = mf ac e
    noteGroupC ac (Beamed cs)   = foldlM noteGroupC ac cs
    noteGroupC ac (Tuplet _ es) = foldlM mf ac es


-- | Do not expose this as it is too general / complex.
--
genTransform :: forall st p1 p2 d1 d2 a1 a2. 
                (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
             -> st
             -> Section p1 d1 a1
             -> Section p2 d2 a2
genTransform elemT st0 ph = 
    evalRewrite (partT ph) default_section_info st0
  where

    partT :: Section p1 d1 a1 -> Mon st (Section p2 d2 a2) 
    partT (Section name info bs)        = 
        local (const info) (Section name info <$> mapM noteGroupT bs)

    noteGroupT :: NoteGroup p1 d1 a1 -> Mon st (NoteGroup p2 d2 a2)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Beamed cs)      = Beamed      <$> mapM noteGroupT cs
    noteGroupT (Tuplet spec es) = Tuplet spec <$> mapM elemT es



--
-- Design note - this leaks /shape/, possible to change a rest 
-- to a note or vice-versa.
-- 
-- However, this functionality is for /library writers/ not
-- /top level users/ where it seems that acknowledging the 
-- note-rest distinction is useful.
-- 
-- Also it allows us to use element for Maybe when calculating 
-- contours.
--

data CadenzaAlgo st pch1 pch2 drn1 drn2 anno1 anno2 = CadenzaAlgo
    { initial_state     :: st
    , element_trafo     :: 
            Element pch1 drn1 anno1 -> Mon st (Element pch2 drn2 anno2)
    }


transformCadenza :: CadenzaAlgo st pch1 pch2 drn1 drn2 anno1 anno2
                 -> Section pch1 drn1 anno1 
                 -> Section pch2 drn2 anno2
transformCadenza  (CadenzaAlgo { initial_state = st0 
                               , element_trafo = elemT }) = 
    genTransform elemT st0



-- | This is a seems less generally useful than @transformP@ 
-- so we don\'t expose an /algo/.
-- 
collectP :: forall st pch drn anno ac.
            (ac -> pch -> Mon st ac) 
         -> ac 
         -> st
         -> Section pch drn anno 
         -> ac
collectP mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note p _ _ _)      = mf ac p
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac



--------------------------------------------------------------------------------
-- Transformation

mapPitch :: (pch1 -> pch2) -> Section pch1 drn anno -> Section pch2 drn anno
mapPitch fn = ctxMapPitch (\_ p -> fn p)


ctxMapPitch :: (Key -> pch1 -> pch2) 
            -> Section pch1 drn anno 
            -> Section pch2 drn anno
ctxMapPitch fn = transformCadenza algo 
  where
    algo  = CadenzaAlgo { initial_state = ()
                        , element_trafo = stepE 
                        }

    stepE (Note p d a t)    = (\ks -> Note (fn ks p) d a t) <$> asks section_key
    stepE (Rest d)          = pure $ Rest d
    stepE (Spacer d)        = pure $ Spacer d
    stepE (Skip d)          = pure $ Skip d
    stepE (Punctuation s)   = pure $ Punctuation s


foldPitch :: (ac -> pch -> ac) -> ac -> Section pch drn anno -> ac
foldPitch fn a0 ph = collectP step a0 () ph
  where
    step ac p   = pure $ fn ac p

--------------------------------------------------------------------------------
-- Duration


-- | This is a seems less generally useful than @transformD@ 
-- so we don\'t expose an /algo/.
-- 
collectD :: forall st pch drn anno ac.
            (ac -> drn -> Mon st ac) 
         -> ac 
         -> st
         -> Section pch drn anno 
         -> ac
collectD mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note _ d _ _)      = mf ac d
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac

--------------------------------------------------------------------------------
-- Transformation

-- Note - increasing or decreasing duration would imply 
-- recalculating bar lines.

mapDuration :: (drn1 -> drn2) -> Section pch drn1 anno -> Section pch drn2 anno
mapDuration fn = transformCadenza algo 
  where
    algo  = CadenzaAlgo { initial_state = ()
                        , element_trafo = stepE }

    stepE (Note p d a t)        = pure $ Note p (fn d) a t
    stepE (Rest d)              = pure $ Rest (fn d)
    stepE (Spacer d)            = pure $ Spacer (fn d)
    stepE (Skip d)              = pure $ Skip (fn d)
    stepE (Punctuation s)       = pure $ Punctuation s


foldDuration :: (ac -> drn -> ac) -> ac -> Section pch drn anno -> ac
foldDuration fn a0 ph = collectD step a0 () ph
  where
    step ac d   = pure $ fn ac d

--------------------------------------------------------------------------------
-- Annotation



collectA :: forall st pch drn anno ac.
            (ac -> anno -> Mon st ac) 
         -> ac 
         -> st
         -> Section pch drn anno 
         -> ac
collectA mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note _ _ a _)      = mf ac a
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac


--------------------------------------------------------------------------------
-- Transformation


mapAnno :: (anno1 -> anno2) -> Section pch drn anno1 -> Section pch drn anno2
mapAnno fn = transformCadenza algo 
  where
    algo  = CadenzaAlgo { initial_state = ()
                        , element_trafo = stepE }

    stepE (Note p d a t)        = pure $ Note p d (fn a) t
    stepE (Rest d)              = pure $ Rest d
    stepE (Spacer d)            = pure $ Spacer d
    stepE (Skip d)              = pure $ Skip d
    stepE (Punctuation s)       = pure $ Punctuation s


foldAnno :: (ac -> anno -> ac) -> ac -> Section pch drn anno -> ac
foldAnno fn a0 ph = collectA step a0 () ph
  where
    step ac a   = pure $ fn ac a

--------------------------------------------------------------------------------
-- Pitch and Annotation

collectPA :: forall st pch drn anno ac.
             (ac -> pch -> anno -> Mon st ac) 
          -> ac 
          -> st
          -> Section pch drn anno 
          -> ac
collectPA mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note p _ a _)      = mf ac p a
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac



--------------------------------------------------------------------------------
-- Pitch Anno Transformation


mapPitchAnno :: (p1 -> a1 -> (p2,a2)) -> Section p1 drn a1 -> Section p2 drn a2
mapPitchAnno fn = transformCadenza algo 
  where
    algo  = CadenzaAlgo { initial_state = ()
                        , element_trafo = stepE }

    stepE (Note p d a t)    = let (p1,a1) = fn p a in pure $ Note p1 d a1 t
    stepE (Rest d)          = pure $ Rest d
    stepE (Spacer d)        = pure $ Spacer d
    stepE (Skip d)          = pure $ Skip d
    stepE (Punctuation s)   = pure $ Punctuation s

foldPitchAnno :: (ac -> pch -> anno -> ac) -> ac -> Section pch drn anno -> ac
foldPitchAnno fn a0 ph = collectPA step a0 () ph
  where
    step ac p a   = pure $ fn ac p a



--------------------------------------------------------------------------------
-- Punctuation

censorPunctuation :: Section pch drn anno -> Section pch drn anno
censorPunctuation (Section name info bs) = Section name info $ noteGroups bs
  where
    noteGroups gs               = catMaybes $ map noteGroup1 gs

    noteGroup1 (Atom e)         = censor e >>= (return . Atom)
    noteGroup1 (Beamed cs)      = let xs = catMaybes $ map noteGroup1 cs
                                  in if null xs then Nothing 
                                                else Just $ Beamed xs
    noteGroup1 (Tuplet spec es) = let xs = catMaybes $ map censor es
                                  in if null xs then Nothing 
                                                else Just $ Tuplet spec xs


    censor (Punctuation {})     = Nothing
    censor e                    = Just e


--------------------------------------------------------------------------------
-- Markup

censorAnno :: Section pch drn anno -> Section pch drn ()
censorAnno (Section name info gs) = Section name info (map noteGroup1 gs)
  where
    noteGroup1 (Atom e)         = Atom $ changeNote e
    noteGroup1 (Beamed cs)      = Beamed $ map noteGroup1 cs
    noteGroup1 (Tuplet spec es) = Tuplet spec $ map changeNote es

    changeNote (Note p d _ t)   = Note p d () t
    changeNote (Rest d)         = Rest d
    changeNote (Spacer d)       = Spacer d
    changeNote (Skip d)         = Skip d
    changeNote (Punctuation s)  = Punctuation s


--------------------------------------------------------------------------------
-- Skip to rest

skipToRest :: Section pch drn anno -> Section pch drn anno
skipToRest (Section name info gs) = Section name info (map noteGroup1 gs)
  where
    noteGroup1 (Atom e)         = Atom $ changeSkip e
    noteGroup1 (Beamed xs)      = Beamed $ map noteGroup1 xs
    noteGroup1 (Tuplet spec es) = Tuplet spec $ map changeSkip es

    changeSkip (Skip d)         = Rest d
    changeSkip e                = e

