def f(x1: bf16[2, 6]):
let x2 = sin x1
    x3 = cos x1
    x4 = transpose [1, 0] x3
    x5 = dot x2 x4
in  x5
