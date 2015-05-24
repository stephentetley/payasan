{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Arpa
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Arpeggiate chords (pitch lists).
--
--------------------------------------------------------------------------------

module Payasan.Base.Arpa
  ( 

    Arpeggio
  , ArpaPattern
  , arpeggio

  , evalArpeggio
--   , arpeggioFw


  , up
  , down
  , asplayed
  , byIndex
  , upDownN

  ) where


import Payasan.Base.Event
import Payasan.Base.EventList
import Payasan.Base.Internal.Utils


import Data.Monoid
import qualified Data.Set as Set





-- | Reifying operations to a list of instructions means we can 
-- get good behavoir for [ UP, DOWN, ... ]
--
data ASegment = ARP_UP | ARP_DOWN | AS_PLAYED | ARBITRARY [Int]
  deriving (Eq,Ord,Show)
              

data Arpeggio pch = Arpeggio
    { arpa_as_played    :: [pch]
    , arpa_segments     :: ArpaPattern
    }


-- a2Snoc :: ASegment -> Arpeggio pch -> Arpeggio pch
-- a2Snoc s1 a@(Arpeggio { arpa_segments = hf }) = a { arpa_segments = hf `snocH` s1 }


newtype ArpaPattern = ArpaPattern { getArpaPattern :: H ASegment }

instance Monoid ArpaPattern where
  mempty        = ArpaPattern emptyH
  a `mappend` b = ArpaPattern $ getArpaPattern a `appendH` getArpaPattern b

-- Not sure these make musical sense as /transformers/ - maybe we 
-- need to expose an /arpeggio pattern/ type to the user...
-- 
up :: ArpaPattern
up = ArpaPattern $ wrapH $ ARP_UP

down :: ArpaPattern
down = ArpaPattern $ wrapH $ ARP_DOWN

asplayed :: ArpaPattern
asplayed = ArpaPattern $ wrapH $ AS_PLAYED

byIndex :: [Int] -> ArpaPattern
byIndex ixs = ArpaPattern $ wrapH $ ARBITRARY ixs

upDownN :: Int -> ArpaPattern
upDownN i | i <= 0  = mempty
upDownN i           = upDownN (i-1) <> up <> down



-- | Pitchset can be synthesized at render time...
--
arpeggio :: [pch] -> ArpaPattern -> Arpeggio pch
arpeggio ps patn = 
    Arpeggio { arpa_as_played    = ps
             , arpa_segments     = patn
             }


evalArpeggio :: Ord pch => Arpeggio pch -> [pch]
evalArpeggio (Arpeggio { arpa_as_played = ps
                       , arpa_segments  = hf }) = 
    step emptyH $ toListH $ getArpaPattern hf
  where
    pset                                = Set.fromList ps
    longup                              = Set.toAscList pset
    shortup                             = init longup
    longdown                            = Set.toDescList pset
    shortdown                           = init longdown
    
    pitches ixs                         = 
        map (`Set.elemAt` pset) $ filter (< Set.size pset) ixs

    step ac []                          = concat $ toListH ac

    step ac (ARP_UP   : ARP_DOWN : ys)  = let ac1 = ac `snocH` shortup
                                          in step ac1 (ARP_DOWN : ys)

    step ac (ARP_UP   : ys)             = step (ac `snocH` longup) ys
                                          
    step ac (ARP_DOWN : ARP_UP   : ys)  = let ac1 = ac `snocH` shortdown
                                          in step ac1 (ARP_DOWN : ys)

    step ac (ARP_DOWN : ys)             = step (ac `snocH` longdown) ys

    step ac (AS_PLAYED : ys)            = step (ac `snocH` ps) ys

    step ac (ARBITRARY ixs : ys)        = step (ac `snocH` pitches ixs) ys


{-

arpeggioFw :: Ord pch 
           => Arpeggio pch -> ImpulseMap a 
           -> (pch -> a -> EventFw uctx z)
           -> EventList uctx ()
arpeggioFw arpa imap mf = 
    impulsesFw (xzipImpulse (evalArpeggio arpa) imap) $ uncurry mf
                               

-}