
  *case server? of
   { Plus(x,y,res) ⇒ res!(x + y)
   ; Eq(x:Int,y,res) ⇒ res!(x = y)
   ; Neg(x,res)   ⇒ res!(0 - x)
   }
|
  server!Plus(3,4,print)
|
  server!Neg(5,print)
