import Data.Char
length1' xs=sum[1|_<-xs]

doubleMe x = x + x

doubleUs x y=x*2+y*2

doubleSmallNumber x= if x>100
                        then x
                        else x*2
fac::(Integral a)=>a->a
fac 0=1
fac n=n*fac(n-1)

keepLowerCase st = [ c | c <- st, c `elem` ['a'..'z']]

test xs=toUpper (head xs):tail [toLower x|x<-xs]

summ []=0
summ (x:xs)=x+summ xs

lucky::(Integral a)=>a->String
lucky 7="You found number 7"
lucky 1="You found numebr one"
lucky x="you better be dead"



listlength :: (Show a)=>[a]->String
listlength []="The list is Empty"
listlength (x:[])="The list has one element"
listlength (x:y:_)="The list is actually really long"

length'::(Num b)=>[a]->b
length' []=0
length' (_:xs)=1+length' xs

sum'::(Num a)=>[a]->a
sum' []=0
sum' (x:xs)=x+sum' xs

firstletter::String->String
firstletter ""="Empty string, Sorry"
firstletter all@(x:xs)="The first letter of "++all++" is "++[x]

bmiTtell::(RealFloat a)=> a->a->String
bmiTtell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You aren't a human"

max'::(Ord a)=>a->a->a
max' a b
    |a>b=a
    |otherwise=b 

initials :: String-> String -> String   
initials firstname lastname = [f] ++ ". " ++ [l] 
    where (f:_) = firstname
          (l:_) = lastname    

calcBmis :: (RealFloat a)=>[(a,a)]-> [a]
calcBmis xs =[bmi w h| (w, h)<- xs]
    where bmi weight height=weight/height^2   

cyl :: (RealFloat a) => a -> a -> a 
cyl r h =
    let sideArea = 2 * pi * r * h 
        topArea  = pi * r ^2    
    in  sideArea + 2 * topArea

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] ->[a]
take' n _
    | n <= 0 =[]
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs    

reverse' :: [a] -> [a]
reverse' []= []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a ->[a]
repeat' x = x: repeat'  x

largestDivisible :: (Integral a)=> a
largestDivisible=head(filter div' [100000,99999..])
    where div' x=x `mod` 2341==0


make1:: Char-> Int
make1 x=1

multThree::Num a=>a->a->a->a
multThree x y z=x*y*z  