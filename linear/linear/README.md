This library treats Bits as if they were Vectors and Matrices.
solution5 solves the WoW lights problem for 5 lights.

`inv(l4)*l54`

```
ls =
  [[1, 0, 1, 1, 0], // 22
   [0, 0, 1, 1, 0], // 6
   [1, 1, 0, 0, 0], // 24
   [1, 1, 0, 1, 0]] // 26
```

`727834 = 22 * 2^15 + 6 * 2^10 + 24 * 2^5 + 25 * 2^0`

22 <|> 19::Int => 18

[1, 0, 1, 1, 0] <|> [1, 0, 0, 1, 1] => [1, 0, 0, 1, 0]

map (eval.((<|>) 19)) m => [0,1,1,0] => 6

(fill 4 (19::Int)) <|> 727834 => 592402