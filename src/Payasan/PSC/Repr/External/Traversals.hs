{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.Traversals
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals for External syntax.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.Traversals
  (
    Mon 
  , genTransform
  , genTransformSection
  , genTransformBars
  , asks
  , get
  , put
  
  , liftElementTrafo

  , ExtPitchAlgo(..)
  , transformP

  , ExtDurationAlgo(..)
  , transformD

  , ExtPitchAnnoAlgo(..)
  , transformPA

  -- Alternative...
  , ExternalAlgo(..)
  , transformExternal
  , mapPitch
  , mapDuration
  , mapAnno

  ) where



import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.SyntaxCommon

import Control.Monad.Reader                     -- package: mtl
import Control.Monad.State


  
 
--
-- These traversals are essentially maps (elementary, shape 
-- preserving, non-failing)
--
-- State is necessary.
--
-- SectionInfo should be accessible.
-- 

-- data TravM st a = TravM { getTravM :: st -> (st,a) }
type TravM st a = State st a
type ElemM st a = ReaderT SectionInfo (State st) a


type Mon st a = ElemM st a




genTransform :: (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
             -> st
             -> Part p1 d1 a1
             -> Part p2 d2 a2
genTransform elemT st ph = evalState (partT elemT ph) st




genTransformSection :: (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
                    -> st
                    -> Section p1 d1 a1
                    -> Section p2 d2 a2
genTransformSection elemT st se = evalState (sectionT elemT se) st




genTransformBars :: (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
                 -> SectionInfo
                 -> st 
                 -> [Bar p1 d1 a1]
                 -> [Bar p2 d2 a2]
genTransformBars elemT info st bs = 
    evalState (runReaderT (mapM (barT elemT) bs) info) st




partT :: (Element p1 d1 a1 -> ElemM st (Element p2 d2 a2)) 
       -> Part p1 d1 a1 -> TravM st (Part p2 d2 a2)
partT elemT (Part ss)               = Part <$> mapM (sectionT elemT) ss


sectionT :: (Element p1 d1 a1 -> ElemM st (Element p2 d2 a2)) 
         -> Section p1 d1 a1 
         -> TravM st (Section p2 d2 a2)
sectionT elemT (Section name info bs) = 
    Section name info <$> runReaderT (mapM (barT elemT) bs) info


barT :: (Element p1 d1 a1 -> ElemM st (Element p2 d2 a2)) 
     -> Bar p1 d1 a1 -> ElemM st (Bar p2 d2 a2)
barT elemT (Bar cs)                 = Bar <$> mapM (noteGroupT elemT) cs


noteGroupT :: (Element p1 d1 a1 -> ElemM st (Element p2 d2 a2)) 
           -> NoteGroup p1 d1 a1 -> ElemM st (NoteGroup p2 d2 a2)
noteGroupT elemT (Atom e)           = Atom <$> elemT e
noteGroupT elemT (Beamed cs)        = Beamed <$> mapM (noteGroupT elemT) cs
noteGroupT elemT (Tuplet spec cs)   = Tuplet spec <$> mapM (noteGroupT elemT) cs




--------------------------------------------------------------------------------
-- Lift a pure Element transformer

liftElementTrafo :: (Element p1 d1 a1 -> Element p2 d2 a2) 
                 -> Element p1 d1 a1 
                 -> Mon st (Element p2 d2 a2)
liftElementTrafo f = \e -> return (f e)



--------------------------------------------------------------------------------
-- Pitch

data ExtPitchAlgo st pch1 pch2 = ExtPitchAlgo 
    { initial_stateP :: st
    , element_trafoP :: forall drn anno. 
                        Element pch1 drn anno -> Mon st (Element pch2 drn anno)
    }


transformP :: forall st p1 p2 drn anno.
              ExtPitchAlgo st p1 p2 
           -> Part p1 drn anno 
           -> Part p2 drn anno
transformP (ExtPitchAlgo { initial_stateP = st0 
                          , element_trafoP = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Duration

data ExtDurationAlgo st drn1 drn2 = ExtDurationAlgo 
    { initial_stateD :: st
    , element_trafoD :: forall pch anno. 
                        Element pch drn1 anno -> Mon st (Element pch drn2 anno)
    }


transformD :: forall st pch d1 d2 anno.
              ExtDurationAlgo st d1 d2 
           -> Part pch d1 anno 
           -> Part pch d2 anno
transformD (ExtDurationAlgo { initial_stateD = st0 
                            , element_trafoD = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Pitch and Anno

data ExtPitchAnnoAlgo st pch1 anno1 pch2 anno2 = ExtPitchAnnoAlgo 
    { initial_statePA :: st
    , element_trafoPA :: 
             forall drn. 
             Element pch1 drn anno1 -> Mon st (Element pch2 drn anno2)
    }


transformPA :: forall st p1 p2 drn a1 a2.
               ExtPitchAnnoAlgo st p1 a1 p2 a2
            -> Part p1 drn a1 
            -> Part p2 drn a2
transformPA (ExtPitchAnnoAlgo { initial_statePA = st0 
                               , element_trafoPA = elemT }) = 
    genTransform elemT st0



--------------------------------------------------------------------------------
-- External algo

-- DESIGN NOTE
-- Alternative - a single element changing transform algo.
-- Can we derive (all) other transformations from it? 


data ExternalAlgo st pch1 pch2 drn1 drn2 anno1 anno2 = ExternalAlgo 
    { initial_state :: st
    , element_trafo :: 
            Element pch1 drn1 anno1 -> Mon st (Element pch2 drn2 anno2)
    }


transformExternal :: ExternalAlgo st pch1 pch2 drn1 drn2 anno1 anno2
                  -> Part pch1 drn1 anno1 
                  -> Part pch2 drn2 anno2
transformExternal  (ExternalAlgo { initial_state = st0 
                                 , element_trafo = elemT }) = 
    genTransform elemT st0


-- NOTE - mapping traversals are expected for unit changing.
-- Musical transformations may require more knowledge of 
-- structure, e.g whether we are changing notes in a chord or 
-- a note, etc.

mapPitch :: (pch1 -> pch2) 
         -> Part pch1 drn anno 
         -> Part pch2 drn anno
mapPitch f = 
    transformExternal (ExternalAlgo { initial_state = ()
                                    , element_trafo = liftElementTrafo g })
  where
    g (Note p d a t)            = Note (f p) d a t
    g (Rest d)                  = Rest d
    g (Spacer d)                = Spacer d
    g (Skip d)                  = Skip d
    g (Chord ps d a t)          = Chord (map f ps) d a t
    g (Graces ns)               = Graces $ map h ns
    g (Punctuation s)           = Punctuation s

    h (Grace1 p d)              = Grace1 (f p) d
    
-- TODO - sharing "Note" between NoteElem and Graces is too 
-- finicky, change syntax at some point.



mapDuration :: (drn1 -> drn2) 
            -> Part pch drn1 anno 
            -> Part pch drn2 anno
mapDuration f = 
    transformExternal (ExternalAlgo { initial_state = ()
                                    , element_trafo = liftElementTrafo g })
  where
    g (Note p d a t)            = Note p (f d) a t
    g (Rest d)                  = Rest $ f d
    g (Spacer d)                = Spacer $ f d
    g (Skip d)                  = Skip $ f d
    g (Chord ps d a t)          = Chord ps (f d) a t
    g (Graces ns)               = Graces $ map h ns
    g (Punctuation s)           = Punctuation s

    h (Grace1 p d)              = Grace1 p (f d)


mapAnno :: (anno1 -> anno2) 
        -> Part pch drn anno1
        -> Part pch drn anno2
mapAnno f = 
    transformExternal (ExternalAlgo { initial_state = ()
                                    , element_trafo = liftElementTrafo g })
  where
    g (Note p d a t)            = Note p d (f a) t
    g (Rest d)                  = Rest d
    g (Spacer d)                = Spacer d
    g (Skip d)                  = Skip d
    g (Chord ps d a t)          = Chord ps d (f a) t
    g (Graces ns)               = Graces ns
    g (Punctuation s)           = Punctuation s

