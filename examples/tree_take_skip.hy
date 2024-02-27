  *all?t.{ flip!t | flop!t }
| *case flip? of
   { Leaf        ⇒ {}
   ; Node(c,l,r) ⇒ c!0 | flip!l | flop!r }
| *case flop? of
   { Leaf        ⇒ {}
   ; Node(c,l,r) ⇒ flop!l | flip!r }
| all!t
