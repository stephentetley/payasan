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
