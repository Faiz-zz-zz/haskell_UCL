import Data.Char

mult::[Int]-> Int
mult xs= foldr (*) 1 xs

posList:: [Int]->[Int]
posList xs=filter (>0) xs

trueList::[Bool]->Bool
trueList xs=foldr (==) True xs

evenList:: [Int]->Bool
evenList xs =  trueList [if x `mod` 2==0 then True else False | x<-xs]

maxList:: [Int]->Int
maxList xs= foldr max (head xs) xs

inRange:: Int -> Int -> [Int] -> [Int]
inRange a b xs= filter (<b) (filter (>a) xs)

countPositives:: [Int]->Int
countPositives xs =foldr (+) 0 [1|x<-posList xs ]

myLength:: String -> Int
myLength str=foldr (+) 0 (map make1 str)
	where make1 x=1

myMap:: (a->b)->[a]->[b]
myMap f xs=	[f x|x<-xs]

myLength'::String->Int
myLength' str=foldr (+) 0 [1|x<-str]	