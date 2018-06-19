length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


--Guards

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "Qechacha"
  | bmi <= 25 = "Yihun"
  | bmi <= 30 = "Dubister"
  | otherwise = "Asa Nebari"

max' :: (Ord a) => a -> a -> a
max' x y 
  | x > y = x
  | otherwise = y


mycompare :: (Ord a) => a -> a -> Ordering
a `mycompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT 


bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | bmi <= skinny = "Qechacha"
  | bmi <= normal = "Norme"
  | bmi <  fatso  = "Dube"
  | otherwise = "Asa Nebari"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25
        fatso = 30



initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."  -- initials (f:_) (l:) = ...
  where (f:_) = firstName
        (l:_) = lastName        

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
  where bmi weight height = weight / height ^ 2

length'' :: (Num b) => [a] -> b
length'' xs = case xs of [] -> 0
                         (_:xs) -> 1 + length'' xs          