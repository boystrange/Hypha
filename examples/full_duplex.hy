
  *node?(x, y).{
     new a in {
       x!a | y?z.node!(a, z)
     }
  }
| node!(e,f) | node!(f,e)
