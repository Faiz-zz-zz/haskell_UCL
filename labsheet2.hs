import Data.Char


inRange :: Int->Int->[Int]->[Int]
inRange x y []=[]
inRange x y (a:xs)
    | a>x && a<y=[a]++ inRange x y xs
    | otherwise= inRange x y xs

countPositives::[Int]->Int
countPositives []=0
countPositives (x:xs)
    | x>0 = 1 + countPositives xs
    | otherwise= countPositives xs

capitalised :: String-> String
capitalised (x:xs) 
    | length (x:xs) < 4 = (x:xs)
    | otherwise = toUpper x : lower xs
        where lower []=[] 
              lower (x:xs)=toLower x : lower xs

title :: [String]->[String]
title []=[]
title (x:xs)=[capitalised x]++title xs

--isort :: (Ord a) => [a] -> [a]
--insertionSort xs = foldr i

insert :: Int -> [Int] ->[Int]
insert x []=[x]
insert x (y:xs)=if x<y then x:y:xs
                else y: insert x xs

isort:: [Int]->[Int]
isort [x]=[x]
isort (x:xs)= insert x (isort xs)	

merge ::(Ord a)=>[a] -> [a] -> [a]
merge xs []=xs
merge [] xs=xs
merge (x:xs) (y:xt)= if x<y then (x:(merge xs (y:xt))) 
                     else (y:merge (x:xs) xt)

splithalf::[a]->([a], [a])
splithalf [] = ([], [])
splithalf [a] = ([a], [])
splithalf xs = (first xs, second xs)
      where 
    first = take (length xs `div` 2)
    second = drop (length xs `div` 2)

msort ::(Ord a) => [a] -> [a]
msort [x]=[x]
msort xs= merge (msort (fst (splithalf (xs)))) (msort (snd (splithalf (xs))))

rotor::Int->[Char]->[Char]
rotor a xs = if a<0 then "can't do this shit"
             else drop a xs++take a xs

succ' :: Char-> Char
succ' 'Z' = 'A'
succ' 'z' = 'a'
succ' a = if a `elem` ['0'..'9'] then a else succ a


encipher :: Int -> Char -> Char
encipher n a =  iterate succ' a !! n 


normalise :: String -> String
normalise xs = map toUpper (filter (/=' ') xs)

encipherStr :: Int -> String -> String
encipherStr n xs = [encipher n x|x<-normalise xs]

pred' :: Char-> Char
pred' 'A' = 'Z'
pred' 'a' = 'z'
pred' a = if a `elem` ['0'..'9'] then a else pred a


decipher :: Int -> Char -> Char
decipher n a =  iterate pred' a !! n

decrypt :: String -> [String]
decrypt xs= [[decipher n x|x<-xs]|n<-[1..26]]
