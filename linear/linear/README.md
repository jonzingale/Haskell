This library treats Bits as if they were Vectors and Matrices.
solution5 solves the WoW lights problem for 5 lights.

```
solution5 n = 727834 |> (31 <> n)
solution5 v =
  let ls = inv(l4)*l54 in
  ls([1,1,1,1,1] - v)
```

```
ls =
  [[1, 0, 1, 1, 0], // 22
   [0, 0, 1, 1, 0], // 6
   [1, 1, 0, 0, 0], // 24
   [1, 1, 0, 1, 0]] // 26
```

`727834 = 22 * 2^15 + 6 * 2^10 + 24 * 2^5 + 25 * 2^0`