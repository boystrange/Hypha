def node0?(a1 : [_]{1,0}, b1 : [_]{0,1}) =
	new b'1 : [_]{1,1} in
	new c1 : [_]{1,1} in
	{
		(b1 : [_]{0,1})!(b'1 : [_]{1,0}) |
		(a1 : [_]{1,0})?a'1 : [_]{1,0}.(c1 : [_]{0,1})!(a'1 : [_]{1,0}) |
		(c1 : [_]{1,0})?a'1 : [_]{1,0}.
		node0!((a'1 : [_]{1,0}), (b'1 : [_]{0,1}))
	}
 and node1?(a1 : [_]{1,0}, b1 : [_]{0,1}, a2 : [_]{1,0}, b2 : [_]{0,1}) =
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
	node0!(c0_1, c1_0) | 
	node0!(c15_14, c14_15) | 
	node1!(c10_9, c9_10, c10_11, c11_10) | 
	node1!(c11_10, c10_11, c11_12, c12_11) | 
	node1!(c12_11, c11_12, c12_13, c13_12) | 
	node1!(c13_12, c12_13, c13_14, c14_13) | 
	node1!(c14_13, c13_14, c14_15, c15_14) | 
	node1!(c1_0, c0_1, c1_2, c2_1) | 
	node1!(c2_1, c1_2, c2_3, c3_2) | 
	node1!(c3_2, c2_3, c3_4, c4_3) | 
	node1!(c4_3, c3_4, c4_5, c5_4) | 
	node1!(c5_4, c4_5, c5_6, c6_5) | 
	node1!(c6_5, c5_6, c6_7, c7_6) | 
	node1!(c7_6, c6_7, c7_8, c8_7) | 
	node1!(c8_7, c7_8, c8_9, c9_8) | 
	node1!(c9_8, c8_9, c9_10, c10_9)
}
