
  *producer?(x, n).
   new a in { x!(n, a) | producer!(a, n + 1) }
|
  *consumer?x.x?(y, z).{ print!y | consumer!z }
|
  producer!(b,0) | consumer!b
