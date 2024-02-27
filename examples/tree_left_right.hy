
  *all?t.{ left!t | right!t }
|
  *left?t.
   case t of
   { Leaf        ⇒ {}
   ; Node(c,l,_) ⇒ c!0 | left!l }
|
  *right?t.
   case t of
   { Leaf        ⇒ {}
   ; Node(c,l,r) ⇒ right!l | all!r }
|
  all!t
