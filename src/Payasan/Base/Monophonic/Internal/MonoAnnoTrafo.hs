{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.MonoAnnoTrafo
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversal of Mono syntax for annotation transformation.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.MonoAnnoTrafo
  (
    Mon 
  , MonoAnnoAlgo(..)
  , transform
  , mapAnno
  ) where



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.RewriteMonad


type Mon st a = Rewrite st a

data MonoAnnoAlgo st anno1 anno2 = MonoAnnoAlgo 
    { initial_state :: st
    , element_trafo :: forall pch drn. 
                       Element pch drn anno1 -> Mon st (Element pch drn anno2)
    }


transform :: MonoAnnoAlgo st a1 a2
          -> Phrase pch drn a1 
          -> Phrase pch drn a2
transform algo ph = evalRewriteDefault (phraseT algo ph) (initial_state algo)

phraseT :: MonoAnnoAlgo st a1 a2 
        -> Phrase pch drn a1 
        -> Mon st (Phrase pch drn a2) 
phraseT algo (Phrase bs)          = Phrase <$> mapM (barT algo) bs



barT :: MonoAnnoAlgo st a1 a2 -> Bar pch drn a1 -> Mon st (Bar pch drn a2)
barT algo (Bar info cs)           = local info $ 
    Bar info <$> mapM (noteGroupT algo) cs

  
noteGroupT :: MonoAnnoAlgo st a1 a2 
           -> NoteGroup pch drn a1
           -> Mon st (NoteGroup pch drn a2)
noteGroupT algo (Atom e)          = let elemT = element_trafo algo
                                    in Atom <$> elemT e
noteGroupT algo (Tuplet spec cs)  = Tuplet spec <$> mapM (noteGroupT algo) cs



--------------------------------------------------------------------------------
-- Transformation


mapAnno :: (anno1 -> anno2) -> Phrase pch drn anno1 -> Phrase pch drn anno2
mapAnno fn = transform algo 
  where
    algo  = MonoAnnoAlgo { initial_state    = ()
                         , element_trafo    = stepE 
                         }

    stepE (Note p d a)  = pure $ Note p d (fn a)
    stepE (Rest d)      = pure $ Rest d

