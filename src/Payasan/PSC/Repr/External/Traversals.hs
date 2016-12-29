{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.Traversals
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

  , BeamPitchAlgo(..)
  , transformP

  , BeamDurationAlgo(..)
  , transformD

  , BeamPitchAnnoAlgo(..)
  , transformPA

  ) where



import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.SyntaxCommon



genTransform :: (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
             -> st
             -> Part p1 d1 a1
             -> Part p2 d2 a2
genTransform elemT st ph = evalTravM st (partT elemT ph)



genTransformSection :: (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
                    -> st
                    -> Section p1 d1 a1
                    -> Section p2 d2 a2
genTransformSection elemT st se = evalTravM st (sectionT elemT se)



genTransformBars :: (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
                 -> SectionInfo
                 -> st 
                 -> [Bar p1 d1 a1]
                 -> [Bar p2 d2 a2]
genTransformBars elemT info st bs = 
    evalTravM st (withSectionInfo info (mapM (barT elemT) bs))
  

--
-- TODO - Rewrite Monad is not the right basis for these 
-- traversals. 
--
-- They are essentially maps (elementary, shape preserving, 
-- non-failing)
--
-- State is necessary.
--
-- SectionInfo should be accessible.
-- 

data TravM st a = TravM { getTravM :: st -> (st,a) }
data ElemM st a = ElemM { getElemM  :: SectionInfo -> st -> (st,a) }

type Mon st a = ElemM st a

instance Functor (TravM st) where
  fmap f ma = TravM $ \st -> let (s1,a) = getTravM ma st in (s1, f a)

instance Functor (ElemM st) where
  fmap f ma = ElemM $ \r st -> let (s1,a) = getElemM ma r st in (s1, f a)

instance Applicative (TravM st) where
  pure a    = TravM $ \st -> (st,a)
  mf <*> ma = TravM $ \st -> 
                let (s1,f) = getTravM mf st
                    (s2,a) = getTravM ma s1
                in (s2, f a)


instance Applicative (ElemM st) where
  pure a    = ElemM $ \_ st -> (st,a)
  mf <*> ma = ElemM $ \r st -> 
                let (s1,f) = getElemM mf r st
                    (s2,a) = getElemM ma r s1
                in (s2, f a)



instance Monad (TravM st) where
  return    = pure
  ma >>= k  = TravM $ \st -> 
                let (s1,a) = getTravM ma st in getTravM (k a) s1

instance Monad (ElemM st) where
  return    = pure
  ma >>= k  = ElemM $ \r st -> 
                let (s1,a) = getElemM ma r st in getElemM (k a) r s1

evalTravM :: st -> TravM st a -> a
evalTravM st ma = let (_,a) = getTravM ma st in a


withSectionInfo :: SectionInfo -> ElemM st a -> TravM st a
withSectionInfo info ma = TravM $ \st -> getElemM ma info st

asks :: (SectionInfo -> a) -> Mon st a
asks proj = ElemM $ \r st -> (st, proj r)


get :: Mon st st
get = ElemM $ \_ st -> (st,st)


put :: st -> Mon st ()
put st = ElemM $ \_ _ -> (st,())



partT :: (Element p1 d1 a1 -> ElemM st (Element p2 d2 a2)) 
       -> Part p1 d1 a1 -> TravM st (Part p2 d2 a2)
partT elemT (Part ss)               = Part <$> mapM (sectionT elemT) ss


sectionT :: (Element p1 d1 a1 -> ElemM st (Element p2 d2 a2)) 
         -> Section p1 d1 a1 
         -> TravM st (Section p2 d2 a2)
sectionT elemT (Section name info bs) = 
    Section name info <$> withSectionInfo info (mapM (barT elemT) bs)


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
           -> Part p1 drn anno 
           -> Part p2 drn anno
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
           -> Part pch d1 anno 
           -> Part pch d2 anno
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
            -> Part p1 drn a1 
            -> Part p2 drn a2
transformPA (BeamPitchAnnoAlgo { initial_statePA = st0 
                               , element_trafoPA = elemT }) = 
    genTransform elemT st0
