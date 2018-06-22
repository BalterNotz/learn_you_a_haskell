-- All functions take one argument in haskell

multThree :: (Num a) => a -> a -> a -> a
multThree a b c = a * b * c

multTwoWithNine = multThree 9 -- (a -> a -> a) = 9 * a * b

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred n = compare 100 n

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f as bs = case (f,as,bs) of (_,[],_) -> []
                                     (_,_,[]) -> []
                                     (f,(a:as),(b:bs)) -> f a b : zipWith' f as bs
--OR
-- zipWith' _ [] _ = []
-- zipWith' _ _ [] = []
-- zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs


