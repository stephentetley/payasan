{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Melody01 where


import Payasan.Base
import Payasan.Base.Names.GeneralMidiInstruments

import Payasan.Models.Melody
import Payasan.Models.Melody.Transform

import Data.Monoid ( (<>) )

-- ----------- TEMP ----------  
import qualified Payasan.Base.Internal.Pachet as PACHET
import Payasan.Base.Internal.Pitch
import Payasan.Base.Names.Pitch
import Data.List


-- test01 = parsetest "M:4/2"
-- test02 = parsetest "4/2"

fmajor :: Melody
fmajor = [melody| M:4/4 \\ F G A _B c d e |]


demo01 :: IO ()
demo01 = writeMF0 "out/vibes01.mid" $ renderMelodyPhrase vibesNote (mkTconfig vibraphone) $ 
    localize (set_bpm 180) $ melodyPhrase $ melody01 <> melody02 <> melody03


demo02 :: IO ()
demo02 = writeMF0 "out/accordion01.mid" $ renderMelodyPhrase accordionNote (mkTconfig accordion) $ 
    localize (set_bpm 180) $ melodyPhrase $ melody01 <> melody02 <> melody03



melody01 :: Melody
melody01 = [melody| M:4/4 \\ D A F D |]

melody02 :: Melody
melody02 = augment melody01

melody03 :: Melody
melody03 = diminute melody01

mkTconfig :: Int -> TrackData
mkTconfig pc = TrackData { channel_number    = 1
                         , program_change    = Just pc
                         , generic_text      = ""
                         , sequence_name     = ""
                         , instrument_name   = ""
                         }


-- Note - not sure fixed width is working how you expect...
vibesNote :: MelodyF
vibesNote = makeMelodyF $ \pch ->
    makeEventFw () 2.0 (NoteValue { note_pitch    = pitchToMidi pch
                                  , note_velo_on  = 65
                                  , note_velo_off = 65
                                  })


accordionNote :: MelodyF 
accordionNote = makeMelodyF $ \pch ->
    makeEvent () (NoteValue { note_pitch    = pitchToMidi pch
                            , note_velo_on  = 65
                            , note_velo_off = 65
                            })


-- ----------- TEMP --------------

-- transposeScalar :: Int -> Scale -> Melody -> Melody


-- scalar1 :: Int -> Scale -> Pitch -> Pitch

type Scale = [Z12]

scale :: [Z12] -> Scale
scale = sort . nub

fmajor' :: Scale
fmajor' = scale $ [5,7,9,10,0,2,4]


scalarSucc :: Scale -> Pitch -> Pitch
scalarSucc xs p = let (o,i) = deconsPitch p in p


majorScale :: Pitch -> [Z12]
majorScale p = let (_,i) = deconsPitch p
               in sort $ scanl (+) (fromIntegral i) [2,2,1,2,2,2]

{-

-- List is sorted and non-empty
--
upN :: Int -> Int -> [Int] -> Int
upN start n xs = locate $ cycle xs
  where
    locate (y:ys) | y >= start = iter (n-1) ys
                  | otherwise  = locate ys 
    locate []                  = error "unreachable"
    iter i (y:ys) | i > 0      = iter (i-1) ys
                  | otherwise  = y
    iter i []                  = error "unreachable"


test01 = upN 0 2 [0,2,4,5,7,8,10]
test02 = upN 4 2 [0,2,4,5,7,8,10]
test03 = upN 7 2 [0,2,4,5,7,8,10]

-}