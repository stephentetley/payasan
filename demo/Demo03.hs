{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo03 where


import Payasan.Base.Duration
import Payasan.Base.Notelist
import Payasan.Base.Pitch

import Payasan.Base.Internal.LilyPond.OutTrans
import Payasan.Base.Internal.LilyPond.Output

import Payasan.Base.Names.Interval
import Payasan.Base.Names.Pitch

phrase01 :: StdPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [lilypond| c4 d e fis | c |]


global_ri :: GlobalRenderInfo
global_ri = GlobalRenderInfo
    { global_pitch_directive = RelPitch middle_c
    }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

test01,test02, test03 :: Int
test01 = octaveCount $ Interval 9 13
test02 = octaveCount $ Interval 1 0
test03 = octaveCount $ Interval 8 12
test04 = simpleIntervalOf $ Interval 8 12

{-

addInterval :: Pitch -> Interval -> Pitch
addInterval (Pitch ss o) iv = Pitch ss1 ov1
  where
    ss1   = pachetAdd ss iv
    ostep = if crossesTwelve ss iv then 1 else 0
    ov1   = o + ostep + octaveCount iv



-- | The algorith provided by Francois Pachet in 
--   An Object-Oriented Representation of Pitch-Classes, 
--   Intervals, Scales and Chords: The basic MusES
-- does not account for octaves:
--
pachetAdd :: PitchSpelling -> Interval -> PitchSpelling
pachetAdd sp0 (Interval { interval_arith_dist = ad
                        , interval_semitones = sc }) = PitchSpelling l1 alt
  where
    (PitchSpelling l _)         = znaturalOf sp0
    znext@(PitchSpelling l1 _)  = PitchSpelling (upwardPL l ad) NAT
    sc_next                     = semitonesToNext sp0 znext
    alt                         = alterationFromDiff $ sc - sc_next


-- | Step 2 in Pachet
upwardPL :: PitchLetter -> Int -> PitchLetter
upwardPL l ad = let n = fromEnum l in toEnum $ (n + (ad - 1)) `mod` 7

downwardPL :: PitchLetter -> Int -> PitchLetter
downwardPL l ad = let n = fromEnum l in toEnum $ (n - (ad - 1)) `mod` 7


alterationFromDiff :: Int -> Alteration
alterationFromDiff i 
    | i >= 2    = DBL_SHARP
    | i == 1    = SHARP
    | i == 0    = NAT
    | i == (-1) = FLAT
    | otherwise = DBL_FLAT


-- | Does the addition of the interval /cross/ into the next 
-- octave?
-- e.g.
--
--   > c(sc=0) => f(sc=5) does not cross
--   > f(sc=5) => c(sc=0) crosses
--
crossesTwelve :: PitchSpelling -> Interval -> Bool
crossesTwelve ps iv = scount >= 12
  where
    scount = zsemitoneCount ps + interval_semitones (simpleIntervalOf iv)
    
-}

{-

addInterval' :: Pitch -> Interval -> Pitch
addInterval' pch@(Pitch (PitchSpelling l a) o) 
                 (Interval { interval_arith_dist = ad
                           , interval_semitones = sc }) = 
    error $ show (ove_root, degree) 
  where
    l1 = upwardPL l ad
    root_semitones    = semitoneCount $ naturalOf pch
    (ove_root,degree) = root_semitones `divMod` 12
    a1 = a
    o1 = o

pitchFromSemitonesLetter :: Int -> PitchLetter -> Pitch
pitchFromSemitonesLetter sc l = error $ show (sc,l, low_ove,low_diff)
  where
    (low_ove,low_diff) = sc `divMod` 12


nearestOve :: Int -> PitchLetter -> Octave
nearestOve sc l = error $ show (sc,l,"~~",low_ove,low_diff,"~~",high_ove,high_diff)
  where
    (low_ove,low_diff)          = sc `divMod` 12
    (high_ove,high_diff)        = (low_ove+1,12-low_diff)


nearestOve :: Int -> PitchSpelling -> Octave
nearestOve sc sp
    | sp == PitchSpelling C FLAT        = ove+1
    | sp == PitchSpelling C DBL_FLAT    = ove+1
    | sp == PitchSpelling B SHARP       = ove-1
    | sp == PitchSpelling B DBL_SHARP   = ove-1
    | otherwise                         = ove
  where
    ove          = sc `div` 12

-}