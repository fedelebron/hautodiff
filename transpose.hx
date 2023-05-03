def f(x1: bf16[2, 3], x8: bf16[2, 2]):
let  x2 = sin x1
     x3 = cos x2
     x4 = x2 + x3
     x5 = exp x4
     x6 = transpose [1, 0] x4
     x7 = dot x5 x6
     x9 = x7 + x8
     x10 = reshape [4] x9
in   x10
