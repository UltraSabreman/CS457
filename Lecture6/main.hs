


module RowYourBoat where
import Music


row:: Music
row =   line [c 4 qn [], c 4 qn [], c 4 en [], d 4 en [], e 4 qn []] :+:
        line [e 4 en [], d 4 en [], e 4 en [], f 4 en [], g 4 hn []] :+:
        line [c 5 sn [], c 5 sn [], c 5 sn [], g 4 sn [], g 4 sn [], g 4 sn [],
        e 4 sn [], e 4 sn [], e 4 sn [], c 4 sn [], c 4 sn [], c 4 sn []] :+:
        line [g 4 en [], f 4 en [], e 4 en [], d 4 en [], c 4 hn []]



main =  do { writeMidi row "Music/row"; }
