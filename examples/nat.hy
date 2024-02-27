
  *eval?(x, n, r).
   case x of
   { Zero => r!n
   ; Succ y => eval!(y, n + 1, r) }

