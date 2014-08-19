



revNumFold :: Integer -> Integer
revNumFold input = 

revNum :: Integer -> Integer
revNum input = revNumRec input 0

revNumRec :: Integer -> Integer -> Integer
revNumRec 0 output = output
revNumRec input output = revNumRec d (output * 10 + m)
    where
      (d,m) = divMod input 10
