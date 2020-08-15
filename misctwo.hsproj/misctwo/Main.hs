-- jmh

fac n =
  if n <= 1 then
    1
    else
      n * fac (n-1)
      
-- with guards.
-- haskell will run through the guards in order and take
-- the first true statement

fac1 n
  | n <= 1 = 1
  | otherwise = n * fac1 (n-1)
  
-- otherwise always evaluates to true

-- accumulators

fac2 n = aux n 1
  where
    aux n acc
      | n <= 1 = acc
      | otherwise = aux (n-1) (n*acc)