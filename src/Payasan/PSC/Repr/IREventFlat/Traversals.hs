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


  -- Alternative...
  , FlatAlgo(..)
  , transformFlat
  , mapOnset
  , mapDuration
  , mapAttrs

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




genTransform :: (Event ot1 d1 a1 -> Mon st (Event ot2 d2 a2))
             -> st
             -> Part ot1 d1 a1
             -> Part ot2 d2 a2
genTransform elemT st ph = evalState (partT elemT ph) st




genTransformSection :: (Event ot1 d1 a1 -> Mon st (Event ot2 d2 a2))
                    -> st
                    -> Section ot1 d1 a1
                    -> Section ot2 d2 a2
genTransformSection elemT st se = evalState (sectionT elemT se) st




partT :: (Event ot1 d1 a1 -> TravM st (Event ot2 d2 a2)) 
       -> Part ot1 d1 a1 
       -> TravM st (Part ot2 d2 a2)
partT elemT (Part ss)               = Part <$> mapM (sectionT elemT) ss


sectionT :: (Event ot1 d1 a1 -> TravM st (Event ot2 d2 a2)) 
         -> Section ot1 d1 a1 
         -> TravM st (Section ot2 d2 a2)
sectionT elemT (Section name es) = 
    Section name <$> mapM elemT es



--------------------------------------------------------------------------------
-- Lift a pure Event transformer

liftEventTrafo :: (Event ot1 d1 a1 -> Event ot2 d2 a2) 
               -> Event ot1 d1 a1 
               -> Mon st (Event ot2 d2 a2)
liftEventTrafo f = \e -> return (f e)






--------------------------------------------------------------------------------
-- Flat algo

-- DESIGN NOTE
-- Alternative - a single element changing transform algo.
-- Can we derive (all) other transformations from it? 

data FlatAlgo st onset1 onset2 drn1 drn2 attrs1 attrs2 = FlatAlgo 
    { initial_state :: st
    , event_trafo :: 
            Event onset1 drn1 attrs1 -> Mon st (Event onset2 drn2 attrs2)
    }


transformFlat :: FlatAlgo st onset1 onset2 drn1 drn2 attrs1 attrs2
              -> Part onset1 drn1 attrs1 
              -> Part onset2 drn2 attrs2
transformFlat  (FlatAlgo { initial_state = st0 
                         , event_trafo = elemT }) = 
    genTransform elemT st0



mapOnset :: (onset1 -> onset2) 
         -> Part onset1 drn attrs 
         -> Part onset2 drn attrs
mapOnset f = transformFlat (FlatAlgo { initial_state = ()
                                     , event_trafo = liftEventTrafo g })
  where
    g (Event o d a) = Event (f o) d a



mapDuration :: (drn1 -> drn2) 
            -> Part onset drn1 attrs 
            -> Part onset drn2 attrs
mapDuration f = transformFlat (FlatAlgo { initial_state = ()
                                        , event_trafo = liftEventTrafo g })
  where
    g (Event o d a) = Event o (f d) a


mapAttrs :: (attrs1 -> attrs2) 
         -> Part onset drn attrs1
         -> Part onset drn attrs2
mapAttrs f = transformFlat (FlatAlgo { initial_state = ()
                                     , event_trafo = liftEventTrafo g })
  where
    g (Event o d a) = Event o d (f a)

