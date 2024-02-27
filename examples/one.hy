-- this example shows that the algorithm already performs
-- some simple form of subtyping. one accepts a channel on
-- which it peforms a single output, whereas omega is a channel
-- on which infinitely many outputs are performed, yet one!omega
-- is well typed

  *one?x.x!1
|
  *omega!34
|
  one!omega

