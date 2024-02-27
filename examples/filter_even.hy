
  *filter?(i, o).
  i?x.case x of
      { None         ⇒ o!None()
      ; Some (n, i') ⇒ if n mod 2 = 0 then
                          new o' in { o!Some(n, o') | filter!(i', o') }
		        else
			  filter!(i', o) }

