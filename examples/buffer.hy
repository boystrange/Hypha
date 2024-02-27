
  *buffer?(inp,out).
   inp?(x,inp').
   new out' in { out!(x, out').buffer!(inp', out') }
|
  *buffer2?(inp,out).
   new tmp in
   { buffer!(inp,tmp) | buffer!(tmp,out) }
|
  *user?(n, read, write).
   new write' in
   new write'' in
   write!(n, write').
   write'!(n, write'').
   read?(m, read').
   read'?(m, read'').
   user!(m + 1, read'', write'')
|
  { buffer2!(w, r) | user!(0, r, w) }

