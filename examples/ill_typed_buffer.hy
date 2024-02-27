
  *buffer1?(inp,out).
   inp?inp'.
   new out' in { out!(out').buffer1!(inp', out') }
|
  *buffer2?(inp,out).
   new tmp in
   { buffer1!(inp,tmp) | buffer1!(tmp,out) }
|
  *buffer3?(inp,out).
   new tmp in
   { buffer1!(inp,tmp) | buffer2!(tmp,out) }
|
  *reader?inp.
   inp?inp'.
   reader!inp'
|
  *writer?out.
   new out' in { out!out'.writer!out' }
|
  buffer3!(w0,r0)
|
  w0!w1.w1!w2.w2!w3.reader!r0

