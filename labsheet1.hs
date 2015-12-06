import Data.Char

square:: Int->Int
square x=x*x

pyth:: Int->Int->Int
pyth x y= (square x)+(square y)

isTriple:: Int->Int->Int->Bool
isTriple x y z= (pyth x y)==square z 

isTripleAny::Int->Int->Int->Bool
isTripleAny x y z=if x>y && x>z then isTriple y z x
			else if y>x && y>z then isTriple x z y
			else isTriple x y z

--div, mod::Int->Int->
halfEvens xs=[if (x `mod` 2)==0 then (x `div` 2) else x| x <- xs]

inRange:: Int->Int->[Int]->[Int]
inRange x y xs=[a| a<-xs, a>=x, a<=y]		

countPositives:: [Int]->Int
countPositives xs=sum[1|x<-xs, x>=0]	

capitalised:: String->String
capitalised str=toUpper (head str):map toLower (tail str)

title:: [String]->[String]
title ls=[if length x>0 then capitalised x else x|x<-ls]


