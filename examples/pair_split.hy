-- this example shows that knowing polarities are crucial
-- for deadlock freedom. The input on fst(p) blocks subsequent
-- output on snd(p). The channel in fst(p) is consumed in the
-- input, so its level must not be taken into account while
-- computing the level of snd(p), even though the very same p
-- is used

  *fwd1?p. fst(p)?(x:Int).snd(p)!x
|
  *fwd2?(a,b). a?(x:Int).b!x

