<CsoundSynthesizer>
<CsOptions>
-o dac
</CsOptions>
<CsInstruments>
; Initialize the global variables.
sr      = 44100
kr      = 4410
ksmps   = 10
nchnls  = 1
0dbfs	= 1
zakinit 1, 1


;; wgbowedbar
instr 1
; pos      =         [0, 1]
; bowpress =         [1, 10]
; gain     =         [0.8, 1]
; intr     =         [0,1]
; trackvel =         [0, 1]
; bowpos   =         [0, 1]

  idur    = p3
  iamp    = p4
  ipch    = cpspch(p5)
  iconst  = p6
  kb      line 0.5, idur, 0.1
  kp      line 0.6, idur, 0.7
  kc      line 1, p3, 1

  a1      wgbowedbar iamp, ipch, kb, kp, 0.995, iconst, 0

          out         a1
endin



; metronome tick
instr 100

  idamp = p4
  asig  tambourine .8, 0.01, 30, idamp
  outs asig, asig

endin

</CsInstruments>
<CsScore>
; Part 1
;ins    st      drn     amp    pch    const
[|eventlist|]
;
   
</CsScore>
</CsoundSynthesizer>