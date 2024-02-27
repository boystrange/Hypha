
  *all?t.{ even!t | odd!t }
|
  *case odd? of
   { Leaf        ⇒ {}
   ; Node(c,l,r) ⇒ even!l | even!r }
|
  *case even? of
   { Leaf        ⇒ {}
   ; Node(c,l,r) ⇒ c!0 | odd!l | odd!r }
|
   all!t
