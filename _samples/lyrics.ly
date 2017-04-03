\version "2.18.2"
<<
\new Voice = "rhythm" {
                        \hide Staff.StaffSymbol
                        \hide Staff.Clef
                        \numericTimeSignature
                        \stemDown
                        \absolute
                        \key c \major
                        \time 4/4
                        b'4 b'8[b'] b'4 r |
                        b' b'8[b'] b'2
                      }
\new Lyrics \lyricmode {
                         \override LyricText #'font-size = #-1
                         \override Lyrics.LyricSpace.minimum-distance = #1.4
                         \set associatedVoice = #"rhythm"
                         Shake4 ba8 -- by shake4 __ \skip4 |
                         Shake4 ba8 -- by shake.2
                       }
>>