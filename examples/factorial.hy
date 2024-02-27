*fact?(n, res).
   if n = 0 then res!1 else {
     new a in {
       fact!(n - 1, a) | a?y.res!(n * y)
     }
   }

