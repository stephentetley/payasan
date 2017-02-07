{-# OPTIONS -Wall #-}

module DemoAltPitch where


import Payasan.Base.AltPitch
import Payasan.Base.Pitch
import Payasan.Base.Names.Pitch



demo00 :: Pitch 
demo00 = middle_c


demo00b :: Pitch 
demo00b = a_3

demo01 :: HertzPitch
demo01 = pitchToHertzPitch a_3

demo02 :: PCPitch
demo02 = pitchToPCPitch a_3

demo03 :: OctPitch
demo03 = pitchToOctPitch a_3
