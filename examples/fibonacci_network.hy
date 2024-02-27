
{ *link?(x,y).x?(v,x').new c in y!(v,c).link!(x',c) } |
{ *delay?(n,x,y).new c in y!(n,c).link!(x,c) } | 
{ *add?(x,y,z).x?(v,x').y?(w,y').new c in z!(v+w,c).add!(x',y',c) } |
{ *copy?(x,y,z).x?(v,x').new c in new d in y!(v,c).z!(v,d).copy!(x',c,d) } |
{ *reader?x.x?(v,x').{ print!v | reader!x' } } |
{ reader!out
| add!(e, f, a)
| delay!(1, a, b)
| copy!(b, c, d) -- try swapping c, d to get a deadlock
| copy!(c, e, out)
| delay!(0, d, f)
}



