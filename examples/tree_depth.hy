
{ *max?(x, y, res).if x > y then res!x else res!y }
|
  *depth?(t, res).
   case t of
   { Leaf       ⇒ res!0
   ; Node(l, r) ⇒ new a in
		  new b in
	          { depth!(l, a)
	          | depth!(r, b)
		  | a?x.b?y.max!(x, y, res)
		  }
   }
