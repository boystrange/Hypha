
-- this fails because Hypha doesn't understand that l and m should have the same type

  *append?(l, m, res).
   case l of
   { Nil => res!m
   ; Cons(x : Int,l') => new r in 
                   { append!(l', m, r) | r?tmp.res!Cons(x,tmp) }
   }
| append!(Nil(), Cons(3,Nil()), a)
