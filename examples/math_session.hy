
  *server?s.
   case s? of
   { Quit    ⇒ {}
   ; Plus c1 ⇒ c1?(x,c2).c2?(y,c3).new c4 in { c3!(x + y, c4) | server!c4 }
   ; Eq c1   ⇒ c1?(x:Int,c2).c2?(y,c3).new c4 in { c3!(x = y, c4) | server!c4 }
   ; Neg c1  ⇒ c1?(x,c2).new c3 in { c2!(0 - x, c3) | server!c3 } }
