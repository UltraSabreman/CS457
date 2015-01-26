module MusicNotes where

import Music

main =
  do { writeMidi cScale "Music/cScale"
     ; writeMidi (chord cMajor) "Music/cMajor"
     ; writeMidi (chord cMinor) "Music/cMinor"
     ; writeMidi cMajorArp "Music/cMajArp"
     ; writeMidi cMajorChd "Music/cMajChd"
     ; writeMidi ex4 "Music/ex4"
     ; writeMidi ex5 "Music/ex5"
     ; writeMidi ex6 "Music/ex6"
     ; writeMidi ex7 "Music/ex7"
     ; writeMidi funkGroove "Music/funk"
     }


cScale =
  line [c 4 qn [], d 4 qn [], e 4 qn [],
        f 4 qn [], g 4 qn [], a 4 qn [],
        b 4 qn [], c 5 qn []]


cMajor = [ n 4 hn [] | n <- [c,e,g] ]
cMinor = [ n 4 wn [] | n <- [c,ef, g] ]


cMajorArp = line  cMajor


cMajorChd = chord cMajor



ex4 = line [ chord cMajor, chord cMinor ]

ex5 = cScale :=: (delay dhn cScale)
ex6 = line [line cMajor,Trans 12 (line cMajor)]

nBeatsRest n note =
   line ((take n (repeat note)) ++ [qnr])

ex7 =
  line [e 4 qn [], d 4 qn [], c 4 qn [], d 4 qn [],
        line [ nBeatsRest 3 (n 4 qn []) | n <- [e,d] ],
        e 4 qn [], nBeatsRest 2 (g 4 qn [] ) ]
