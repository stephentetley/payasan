\version "2.18.2"
chordX = \markup {
                   \fret-diagram #"c:6-1-1;6-1;5-1;4-1;3-2;2-3;1-1;"
                 }
chordY = \markup {
                   \fret-diagram #"6-x;5-x;4-o;3-2;2-3;1-2;"
                 }
<<
{
  <f b e' b' e'' f''>1^\chordX |
  <d' b' e'' g''>^\chordY
}
>>