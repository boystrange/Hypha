{ *length?(l, res).
  case l of
  { Nil                ⇒ res!0
  ; Cons(_ : Unit, l') ⇒ new a in { length!(l', a) | a?n.res!(n + 1) } }
| length!(l, print) }
