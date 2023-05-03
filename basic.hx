def f(x1: bf16[2, 8], x2: bf16[2]):
let   x3: bf16[2, 8] = sin x1
      x4: bf16[2, 8] = x1 + x3
      x5: bf16[2, 8] = x3 - x4
      x6 = broadcast [0] [2, 8] x2
      x7 = x6 - x5
      x8 = transpose [1, 0] x7
      x9 = reshape [16] x8
      x10 = constant bf16[] [0]
      x11 = broadcast [] [16] x10
      x12 = x11 - x9
      x13 = reshape [1, 16] x12
      x14 = reshape [16, 1] x8
      x15 = dot x13 x14
in    x15
