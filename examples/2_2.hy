def node1?(a1 : [_]{1,0}, b1 : [_]{0,1}, a2 : [_]{1,0}, b2 : [_]{0,1}) =
	new b'1 : [_]{1,1} in
	new b'2 : [_]{1,1} in
	new c1 : [_]{1,1} in
	new c2 : [_]{1,1} in
	{
		(b1 : [_]{0,1})!(b'1 : [_]{1,0}) |
		(b2 : [_]{0,1})!(b'2 : [_]{1,0}) |
		(a1 : [_]{1,0})?a'1 : [_]{1,0}.(c1 : [_]{0,1})!(a'1 : [_]{1,0}) |
		(a2 : [_]{1,0})?a'2 : [_]{1,0}.(c2 : [_]{0,1})!(a'2 : [_]{1,0}) |
		(c1 : [_]{1,0})?a'1 : [_]{1,0}.(c2 : [_]{1,0})?a'2 : [_]{1,0}.
		node1!((a'1 : [_]{1,0}), (b'1 : [_]{0,1}), (a'2 : [_]{1,0}), (b'2 : [_]{0,1}))
	}
 in {
	node1!(c0'0_0'1, c0'1_0'0, c0'0_1'0, c1'0_0'0) | 
	node1!(c0'1_0'0, c0'0_0'1, c0'1_1'1, c1'1_0'1) | 
	node1!(c1'0_0'0, c0'0_1'0, c1'0_1'1, c1'1_1'0) | 
	node1!(c1'1_0'1, c0'1_1'1, c1'1_1'0, c1'0_1'1)
}
