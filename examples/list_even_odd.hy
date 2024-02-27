
  *all?l.{ odd!l | even!l }
|
  *case odd? of
   { Nil        ⇒ {}
   ; Cons(x, y) ⇒ x!3 | even!y }
|
  *case even? of
   { Nil        ⇒ {}
   ; Cons(_, y) ⇒ odd!y }
|
  all!l

