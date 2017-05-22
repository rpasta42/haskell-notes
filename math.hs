

real = [0, 0.01 ..]
naturals = [0..]

seq = [1, 2, 2.9, 2.99]

p = 3 --limit

[abs (x !! n - p) < e | e <- real, e > 0, n <- getN e]

