maximum' :: (Ord a) => [a] -> a
maximum' xs =  case xs of []  -> error "max of empty list"
                          [x] -> x
                          (x:ys) | x > maxTail -> x   -- | comes after the argument. Syntax is | <predicate> -> expr
                                 | otherwise -> maxTail
                                 where maxTail = maximum' ys

-- maximum' [] = error "max of empty list"
-- maximum' [x] = x
-- maximum' (x:ys) 
--     | x > maxTail = x
--     | otherwise = maxTail
--     | where maxTail = maximum' ys                                  

replicate' :: (Num i, Ord a) => i -> a -> [a]                         
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' n-1 x