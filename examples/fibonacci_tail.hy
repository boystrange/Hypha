
  *fibo?(n, res).{
     new aux in {
       *aux?(k, m, n, res). {
          if k = 0 then res!m
   	  else if k = 1 then res!n
   	  else aux!(k - 1, n, m + n, res)
       }
       | aux!(n, 0, 1, res)
     }
   }
|
  fibo!(10, print)
