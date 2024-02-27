
  *fold?(f,xs,x,r).
   case xs of 
   { Nil => r!x
   ; Cons(y,ys) =>
     new res in
     { fold!(f,ys,x,res)
     | res?tmp.f!(y,tmp,r) } }
|
  *add?(x,y,r).r!(x+y)
|
  *sum?(xs,r).fold!(add,xs,0,r)
