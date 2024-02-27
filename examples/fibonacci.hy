
  *fibo?(n, res).
   if n ≤ 1 then res!n else {
     new a in new b in {
       fibo!(n - 1, a) | fibo!(n - 2, b) | a?y.b?z.res!(y + z)
     }
   }
