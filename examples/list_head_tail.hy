-- EXAMPLE: first sends a message on the first channel, tail sends
-- a message on any subsequent channel

  *case head? of
   { Nil        => {}
   ; Cons(x, y) => x!1 }
|
  *case tail? of
   { Nil        => {}
   ; Cons(x, y) => both!y }
|
  *both?p.{ head!p | tail!p }
|
  both!l
