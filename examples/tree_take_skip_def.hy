
  def all?t = take!t | skip!t
  and take?t = case t of
               { Leaf        ⇒ {}
	       ; Node(c,l,r) ⇒ c!0 | take!l | skip!r }
  and skip?t = case t of
               { Leaf        ⇒ {}
               ; Node(c,l,r) ⇒ skip!l | take!r }
  in
    all!t
