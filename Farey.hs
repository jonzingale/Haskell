-- The nth farey sequence is a list of all fractions
-- between zero and with numerator at most n.
-- fractions magically appear in order and reduced. 
boost (n,[(a,b)]) = [(a,b)]
boost(n,((a,b):(c,d):fractions)) =
    if b+d==n then (a,b):(a+c,b+d):boost(n, (c,d):fractions)
    else (a,b):boost(n,(c,d):fractions)
farey(0) = []
farey(1) = [(0,1),(1,1)]
farey(n) = boost(n,farey(n-1))

