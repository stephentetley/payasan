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
  , mapAccumOnset
  , mapDuration
  , mapBody

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

data FlatAlgo st onset1 onset2 drn1 drn2 body1 body2 = FlatAlgo 
    { initial_state :: st
    , event_trafo :: 
            Event onset1 drn1 body1 -> Mon st (Event onset2 drn2 body2)
    }


transformFlat :: FlatAlgo st onset1 onset2 drn1 drn2 body1 body2
              -> Part onset1 drn1 body1 
              -> Part onset2 drn2 body2
transformFlat  (FlatAlgo { initial_state = st0 
                         , event_trafo = elemT }) = 
    genTransform elemT st0



mapOnset :: (onset1 -> onset2) 
         -> Part onset1 drn body 
         -> Part onset2 drn body
mapOnset f = transformFlat (FlatAlgo { initial_state = ()
                                     , event_trafo = liftEventTrafo g })
  where
    g (Event o d a) = Event (f o) d a



mapAccumOnset :: (st -> onset1 -> (st,onset2))
              -> st 
              -> Part onset1 drn body 
              -> Part onset2 drn body
mapAccumOnset f s0 = transformFlat (FlatAlgo { initial_state = s0
                                             , event_trafo   = mf })
  where
    mf (Event o d a) = get >>= \s -> let (s1,o1) = f s o in put s1 >> return (Event o1 d a)


mapDuration :: (drn1 -> drn2) 
            -> Part onset drn1 body 
            -> Part onset drn2 body
mapDuration f = transformFlat (FlatAlgo { initial_state = ()
                                        , event_trafo = liftEventTrafo g })
  where
    g (Event o d a) = Event o (f d) a


mapBody :: (body1 -> body2) 
        -> Part onset drn body1
        -> Part onset drn body2
mapBody f = transformFlat (FlatAlgo { initial_state = ()
                                    , event_trafo = liftEventTrafo g })
  where
    g (Event o d a) = Event o d (f a)

