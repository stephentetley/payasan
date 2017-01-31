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


  , ExternalAlgo(..)
  , transformExternal
  , mapPitch
  , mapAccumPitch
  , mapDuration
  , mapAnno

  ) where



import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.SyntaxCommon

import Control.Monad.Reader                     -- package: mtl
import Control.Monad.State

import Data.List ( mapAccumL )
  
 
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
    
mapAccumPitch :: (st -> pch1 -> (st,pch2))
              -> st 
              -> Part pch1 drn anno 
              -> Part pch2 drn anno
mapAccumPitch f s0 = 
    transformExternal (ExternalAlgo { initial_state = s0
                                    , element_trafo = mf })
  where
    mf e = get >>= \s -> let (s1,e1) = g s e in put s1 >> return e1

    g s (Note p d a t)          = let (s1,p1) = f s p in (s1, Note p1 d a t)

    g s (Rest d)                = (s, Rest d)

    g s (Spacer d)              = (s, Spacer d)

    g s (Skip d)                = (s, Skip d)

    g s (Chord ps d a t)        = let (s1,ps1) = mapAccumL f s ps 
                                  in (s1, Chord ps1 d a t)

    g s (Graces ns)             = let (s1,ns1) = mapAccumL h s ns 
                                  in (s1, Graces ns1)

    g s (Punctuation a)         = (s, Punctuation a)
                   
    h s (Grace1 p d)            = let (s1,p1) = f s p in (s1, Grace1 p1 d)


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

