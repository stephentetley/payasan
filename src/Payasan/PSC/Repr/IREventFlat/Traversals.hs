{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventFlat.Traversals
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals for IREventFlat syntax.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventFlat.Traversals
  (
    Mon 
  , genTransform
  , genTransformSection
  , asks
  , get
  , put
  
  , liftEventTrafo

  , FlatOnsetAlgo(..)
  , transformO

  , FlatPitchAlgo(..)
  , transformP


  , FlatDurationAlgo(..)
  , transformD

  , FlatAnnoAlgo(..)
  , transformA

  -- Alternative...
  , FlatAlgo(..)
  , transformFlat
  , mapOnset
  , mapPitch
  , mapDuration
  , mapAnno

  ) where



import Payasan.PSC.Repr.IREventFlat.Syntax

import Control.Monad.Reader                     -- package: mtl
import Control.Monad.State


-- Pitch change, e.g. Pitch to CpsPitch, MidiPitch.
-- Duration Change, e.g. Seconds to MidiTicks
 
  
 
--
-- These traversals are essentially maps (elementary, shape 
-- preserving, non-failing)
--
-- State is necessary.
--
-- SectionInfo should be accessible.
-- 

type TravM st a = State st a


type Mon st a = TravM st a




genTransform :: (Event ot1 p1 d1 a1 -> Mon st (Event ot2 p2 d2 a2))
             -> st
             -> Part ot1 p1 d1 a1
             -> Part ot2 p2 d2 a2
genTransform elemT st ph = evalState (partT elemT ph) st




genTransformSection :: (Event ot1 p1 d1 a1 -> Mon st (Event ot2 p2 d2 a2))
                    -> st
                    -> Section ot1 p1 d1 a1
                    -> Section ot2 p2 d2 a2
genTransformSection elemT st se = evalState (sectionT elemT se) st




partT :: (Event ot1 p1 d1 a1 -> TravM st (Event ot2 p2 d2 a2)) 
       -> Part ot1 p1 d1 a1 
       -> TravM st (Part ot2 p2 d2 a2)
partT elemT (Part ss)               = Part <$> mapM (sectionT elemT) ss


sectionT :: (Event ot1 p1 d1 a1 -> TravM st (Event ot2 p2 d2 a2)) 
         -> Section ot1 p1 d1 a1 
         -> TravM st (Section ot2 p2 d2 a2)
sectionT elemT (Section name es) = 
    Section name <$> mapM elemT es



--------------------------------------------------------------------------------
-- Lift a pure Event transformer

liftEventTrafo :: (Event ot1 p1 d1 a1 -> Event ot2 p2 d2 a2) 
               -> Event ot1 p1 d1 a1 
               -> Mon st (Event ot2 p2 d2 a2)
liftEventTrafo f = \e -> return (f e)






--------------------------------------------------------------------------------
-- Onset



data FlatOnsetAlgo st onset1 onset2 = FlatOnsetAlgo 
    { initial_stateO :: st
    , element_trafoO :: forall pch drn anno. 
                        Event onset1 pch drn anno -> Mon st (Event onset2 pch drn anno)
    }


transformO :: forall st onset1 onset2 pch drn anno.
              FlatOnsetAlgo st onset1 onset2 
           -> Part onset1 pch drn anno 
           -> Part onset2 pch drn anno
transformO (FlatOnsetAlgo { initial_stateO = st0 
                          , element_trafoO = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Pitch



data FlatPitchAlgo st pch1 pch2 = FlatPitchAlgo 
    { initial_stateP :: st
    , element_trafoP :: forall onset drn anno. 
                        Event onset pch1 drn anno -> Mon st (Event onset pch2 drn anno)
    }


transformP :: forall st p1 p2 onset drn anno.
              FlatPitchAlgo st p1 p2 
           -> Part onset p1 drn anno 
           -> Part onset p2 drn anno
transformP (FlatPitchAlgo { initial_stateP = st0 
                          , element_trafoP = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Duration

data FlatDurationAlgo st drn1 drn2 = FlatDurationAlgo 
    { initial_stateD :: st
    , element_trafoD :: forall onset pch anno. 
                        Event onset pch drn1 anno -> Mon st (Event onset pch drn2 anno)
    }


transformD :: forall st onset pch d1 d2 anno.
              FlatDurationAlgo st d1 d2 
           -> Part onset pch d1 anno 
           -> Part onset pch d2 anno
transformD (FlatDurationAlgo { initial_stateD = st0 
                             , element_trafoD = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Anno

data FlatAnnoAlgo st anno1 anno2 = FlatAnnoAlgo 
    { initial_stateA :: st
    , element_trafoA :: 
             forall onset pch drn. 
             Event onset pch drn anno1 -> Mon st (Event onset pch drn anno2)
    }


transformA :: forall st onset pch drn a1 a2.
               FlatAnnoAlgo st a1 a2
            -> Part onset pch drn a1 
            -> Part onset pch drn a2
transformA (FlatAnnoAlgo { initial_stateA = st0 
                         , element_trafoA = elemT }) = 
    genTransform elemT st0


--------------------------------------------------------------------------------
-- Flat algo

-- DESIGN NOTE
-- Alternative - a single element changing transform algo.
-- Can we derive (all) other transformations from it? 

data FlatAlgo st onset1 onset2 pch1 pch2 drn1 drn2 anno1 anno2 = FlatAlgo 
    { initial_state :: st
    , event_trafo :: 
            Event onset1 pch1 drn1 anno1 -> Mon st (Event onset2 pch2 drn2 anno2)
    }


transformFlat :: FlatAlgo st onset1 onset2 pch1 pch2 drn1 drn2 anno1 anno2
              -> Part onset1 pch1 drn1 anno1 
              -> Part onset2 pch2 drn2 anno2
transformFlat  (FlatAlgo { initial_state = st0 
                         , event_trafo = elemT }) = 
    genTransform elemT st0



mapOnset :: (onset1 -> onset2) 
         -> Part onset1 pch drn anno 
         -> Part onset2 pch drn anno
mapOnset f = transformFlat (FlatAlgo { initial_state = ()
                                     , event_trafo = liftEventTrafo g })
  where
    g (Event o p d a) = Event (f o) p d a
    g (Grace o p d)   = Grace (f o) p d


mapPitch :: (pch1 -> pch2) 
         -> Part onset pch1 drn anno 
         -> Part onset pch2 drn anno
mapPitch f = transformFlat (FlatAlgo { initial_state = ()
                                     , event_trafo = liftEventTrafo g })
  where
    g (Event o p d a) = Event o (f p) d a
    g (Grace o p d)   = Grace o (f p) d


mapDuration :: (drn1 -> drn2) 
            -> Part onset pch drn1 anno 
            -> Part onset pch drn2 anno
mapDuration f = transformFlat (FlatAlgo { initial_state = ()
                                        , event_trafo = liftEventTrafo g })
  where
    g (Event o p d a) = Event o p (f d) a
    g (Grace o p d)   = Grace o p (f d)


mapAnno :: (anno1 -> anno2) 
        -> Part onset pch drn anno1
        -> Part onset pch drn anno2
mapAnno f = transformFlat (FlatAlgo { initial_state = ()
                                     , event_trafo = liftEventTrafo g })
  where
    g (Event o p d a) = Event o p d (f a)
    g (Grace o p d)   = Grace o p d

