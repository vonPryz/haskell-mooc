-- Exercise set 3a
--
--  * lists
--  * functional programming

module Set3a where

import Mooc.Todo

-- Some imports you'll need.
-- Do not add any other imports! :)
import Data.Char
import Data.Either
import Data.List

------------------------------------------------------------------------------
-- Ex 1: implement the function maxBy that takes as argument a
-- measuring function (of type a -> Int) and two values (of type a).
--
-- maxBy should apply the measuring function to both arguments and
-- return the argument for which the measuring function returns a
-- higher value.
--
-- Examples:
--
--  maxBy (*2)   3       5      ==>  5
--  maxBy length [1,2,3] [4,5]  ==>  [1,2,3]
--  maxBy head   [1,2,3] [4,5]  ==>  [4,5]

maxBy :: (a -> Int) -> a -> a -> a
--maxBy measure a b = todo
maxBy f a b
    | f a > f b = a
    | otherwise = b


------------------------------------------------------------------------------
-- Ex 2: implement the function mapMaybe that takes a function and a
-- Maybe value. If the value is Nothing, it returns Nothing. If it is
-- a Just, it updates the contained value using the function.
--
-- Examples:
--   mapMaybe length Nothing      ==> Nothing
--   mapMaybe length (Just "abc") ==> Just 3

-- mapMaybe f x = todo
-- :{
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just x) = Just (f x)
-- :}

------------------------------------------------------------------------------
-- Ex 3: implement the function mapMaybe2 that works like mapMaybe
-- except it combines two Maybe values using a function of two
-- arguments.
--
-- Examples:
--   mapMaybe2 take (Just 2) (Just "abcd") ==> Just "ab"
--   mapMaybe2 div (Just 6) (Just 3)  ==>  Just 2
--   mapMaybe2 div Nothing  (Just 3)  ==>  Nothing
--   mapMaybe2 div (Just 6) Nothing   ==>  Nothing

-- :{
mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
--mapMaybe2 f x y = todo
mapMaybe2 f Nothing _ = Nothing
mapMaybe2 f _ Nothing = Nothing
mapMaybe2 f (Just x) (Just y) = Just(f x y)
-- :}
------------------------------------------------------------------------------
-- Ex 4: define the functions firstHalf and palindrome so that
-- palindromeHalfs returns the first halfs of all palindromes in its
-- input.
--
-- The first half of a string should include the middle character of
-- the string if the string has an odd length.
--
-- Examples:
--   palindromeHalfs ["abba", "cat", "racecar"]
--     ==> ["ab","race"]
--
-- What types should firstHalf and palindrome have? Give them type
-- annotations.
--
-- Note! Do not change the definition of palindromeHalfs
-- :{
palindromeHalfs :: [String] -> [String]
palindromeHalfs xs = map firstHalf (filter palindrome xs)

palindrome :: String -> Bool
palindrome xs
    | xs == reverse xs = True
    | otherwise = False

firstHalf :: String -> String
firstHalf s = 
    take midpoint s
    where 
        slen = length s
        midpoint =
            if even slen
            then div slen 2
            else div (slen +1) 2

-- :}
-- firstHalf = todo
-- palindrome = todo

------------------------------------------------------------------------------
-- Ex 5: Implement a function capitalize that takes in a string and
-- capitalizes the first letter of each word in it.
--
-- You should probably define a helper function capitalizeFirst that
-- capitalizes the first letter of a string.
--
-- These functions will help:
--  - toUpper :: Char -> Char   from the module Data.Char
--  - words :: String -> [String]
--  - unwords :: [String] -> String
--
-- Example:
--   capitalize "goodbye cruel world" ==> "Goodbye Cruel World"

--capitalize = todo
-- :{
capitalize :: String -> String
capitalize s = 
    unwords $ map capitalizeFirst (words s)

capitalizeFirst :: String -> String
capitalizeFirst s = 
      [h] ++ t
  where
      t = tail s
      h = toUpper $ head s
-- :}

------------------------------------------------------------------------------
-- Ex 6: powers k max should return all the powers of k that are less
-- than or equal to max. For example:
--
-- powers 2 5 ==> [1,2,4]
-- powers 3 30 ==> [1,3,9,27]
-- powers 2 2 ==> [1,2]
--
-- You can assume that k is at least 2.
--
-- Hints:
--   * k^max > max
--   * the function takeWhile

-- :{
powers :: Int -> Int -> [Int]
powers k max = takeWhile (<=max) $ powList k
powList :: Int -> [Int]
powList x = iterate (*x) 1
-- :}

------------------------------------------------------------------------------
-- Ex 7: implement a functional while loop. While should be a function
-- that takes a checking function, an updating function, and an
-- initial value. While should repeatedly apply the updating function
-- to the initial value as long as the value passes the checking
-- function. Finally, the value that doesn't pass the check is
-- returned.
--
-- Examples:
--
--   while odd (+1) 1    ==>   2
--
--   while (<=4) (+1) 0  ==>   5
--
--   let check [] = True
--       check ('A':xs) = False
--       check _ = True
--   in while check tail "xyzAvvt"
--     ==> Avvt

-- while check update value = todo
while :: (a->Bool) -> (a->a) -> a -> a
while check update value =
    if check value == False
    then value 
    else while check update (update value)

------------------------------------------------------------------------------
-- Ex 8: another version of a while loop. This time, the check
-- function returns an Either value. A Left value means stop, a Right
-- value means keep looping.
--
-- The call `whileRight check x` should call `check x`, and if the
-- result is a Left, return the contents of the Left. If the result is
-- a Right, the function should call `check` on the contents of the
-- Right and so on.
--
-- Examples (see definitions of step and bomb below):
--   whileRight (step 100) 1   ==> 128
--   whileRight (step 1000) 3  ==> 1536
--   whileRight bomb 7         ==> "BOOM"
--
-- Hint! Remember the case-of expression from lecture 2.

-- whileRight check x = todo
whileRight :: (a -> Either b a) -> a -> b
whileRight check x =
    case check x of
        Left x -> x
        Right x -> whileRight check x

-- for the whileRight examples:
-- step k x doubles x if it's less than k
step :: Int -> Int -> Either Int Int
step k x = if x<k then Right (2*x) else Left x

-- bomb x implements a countdown: it returns x-1 or "BOOM" if x was 0
bomb :: Int -> Either String Int
bomb 0 = Left "BOOM"
bomb x = Right (x-1)

------------------------------------------------------------------------------
-- Ex 9: given a list of strings and a length, return all strings that
--  * have the given length
--  * are made by catenating two input strings
--
-- Examples:
--   joinToLength 2 ["a","b","cd"]        ==> ["aa","ab","ba","bb"]
--   joinToLength 5 ["a","b","cd","def"]  ==> ["cddef","defcd"]
--
-- Hint! This is a great use for list comprehensions

-- joinToLength = todo
joinToLength :: Int -> [String] -> [String] 
joinToLength n xs =
    let
        cartesianStrings = createCartesian xs xs
        properLengthStrings = [s | s <- cartesianStrings, length s == n] 
    in
       properLengthStrings 
    where
        createCartesian :: [String] -> [String] -> [String]
        createCartesian xs ys =  [x++y | x <- xs, y <- ys]

-- This was pretty tricky; my first attempts were about to get lists of
-- too short strings, calculating suitable matches based on string length and
-- desired length and stuff.
-- Then I realized that let's just make a cartesian product of all the inputs
-- and remove no good elements. This likely isn't that efficient, but we could,
-- say, for large datasets generate AxB for elements that are known to result
-- in strings that aren't too long. Also, filtering the input set might be useful.


------------------------------------------------------------------------------
-- Ex 10: implement the operator +|+ that returns a list with the first
-- elements of its input lists.
--
-- Give +|+ a type signature. NB: It needs to be of the form (+|+) :: x,
-- with the parentheses because +|+ is an infix operator.
--
-- Examples:
--   [1,2,3] +|+ [4,5,6]  ==> [1,4]
--   [] +|+ [True]        ==> [True]
--   [] +|+ []            ==> []
-- :{
(+|+) :: [a] -> [a] -> [a]
[] +|+ [] = []
[] +|+ ys = head ys : []
xs +|+ [] = head xs : [] 
xs +|+ ys = head xs : head ys : []
-- :}
------------------------------------------------------------------------------
-- Ex 11: remember the lectureParticipants example from Lecture 2? We
-- used a value of type [Either String Int] to store some measurements
-- that might be missing. Implement the function sumRights which sums
-- all non-missing measurements in a list like this.
--
-- Challenge: look up the type of the either function. Implement
-- sumRights using the map & either functions instead of pattern
-- matching on lists or Eithers!
--
-- Examples:
--   sumRights [Right 1, Left "bad value", Right 2]  ==>  3
--   sumRights [Left "bad!", Left "missing"]         ==>  0

-- sumRights = todo
sumRights :: [Either a Int] -> Int
sumRights [] = 0
sumRights (x:xs) =
    case x of
        Right i -> i + sumRights xs
        Left _  -> sumRights xs

------------------------------------------------------------------------------
-- Ex 12: recall the binary function composition operation
-- (f . g) x = f (g x). In this exercise, your task is to define a function
-- that takes any number of functions given as a list and composes them in the
-- same order than they appear in the list.
--
-- Examples:
--   multiCompose [] "foo" ==> "foo"
--   multiCompose [] 1     ==> 1
--   multiCompose [(++"bar")] "foo" ==> "foobar"
--   multiCompose [reverse, tail, (++"bar")] "foo" ==> "raboo"
--   multiCompose [(3*), (2^), (+1)] 0 ==> 6
--   multiCompose [(+1), (2^), (3*)] 0 ==> 2


--multiCompose fs = todo
multiCompose :: [(a -> a)] -> a -> a
multiCompose [] x = x
--multiCompose (f:fs) x = multiCompose fs (f x) 
multiCompose fs x = multiCompose' (reverse fs) x

multiCompose' :: [(a -> a)] -> a -> a
multiCompose' [] x = x
multiCompose' (f:fs) x = multiCompose' fs (f x) 

-- This is weird. The results work only if function list is reversed,
-- but that's not what I understood from the textual description;
-- "compose them in the same order than they appear" means, to me,
-- to apply each operator from left to right. Now tests assume
-- that right to left is the right (ha ha) way.

------------------------------------------------------------------------------
-- Ex 13: let's consider another way to compose multiple functions. Given
-- some function f, a list of functions gs, and some value x, define
-- a composition operation that applies each function g in gs to x and then
-- f to the resulting list. Give also the type annotation for multiApp.
--
-- Challenge: Try implementing multiApp without lambdas or list comprehensions.
--
-- Examples:
--   multiApp id [] 7  ==> []
--   multiApp id [id, reverse, tail] "This is a test"
--       ==> ["This is a test","tset a si sihT","his is a test"]
--   multiApp id  [(1+), (^3), (+2)] 1  ==>  [2,1,3]
--   multiApp sum [(1+), (^3), (+2)] 1  ==>  6
--   multiApp reverse [tail, take 2, reverse] "foo" ==> ["oof","fo","oo"]
--   multiApp concat [take 3, reverse] "race" ==> "racecar"
--   multiApp id [head, (!!2), last] "axbxc" ==> ['a','b','c'] i.e. "abc"
--   multiApp sum [head, (!!2), last] [1,9,2,9,3] ==> 6
multiApp f gs x = f (map ($ x) gs)
{-   
multiApp = todo
:{
multiApp :: ([a] -> [a]) -> [(a -> a)] -> a -> [a]
multiApp f [] x = [] 
multiApp f gs x = 
    f h
    where
        h = [g' x | g' <- gs]
:}
-}  
------------------------------------------------------------------------------
-- Ex 14: in this exercise you get to implement an interpreter for a
-- simple language. You should keep track of the x and y coordinates,
-- and interpret the following commands:
--
-- up -- increment y by one
-- down -- decrement y by one
-- left -- decrement x by one
-- right -- increment x by one
-- printX -- print value of x
-- printY -- print value of y
--
-- The interpreter will be a function of type [String] -> [String].
-- Its input is a list of commands, and its output is a list of the
-- results of the print commands in the input.
--
-- Both coordinates start at 0.
--
-- Examples:
--
-- interpreter ["up","up","up","printY","down","printY"] ==> ["3","2"]
-- interpreter ["up","right","right","printY","printX"] ==> ["1","2"]
--
-- Surprise! after you've implemented the function, try running this in GHCi:
--     interact (unlines . interpreter . lines)
-- after this you can enter commands on separate lines and see the
-- responses to them
--
-- The suprise will only work if you generate the return list directly
-- using (:). If you build the list in an argument to a helper
-- function, the surprise won't work.

--interpreter commands = todo
-- :{
interpreter :: [String] -> [String]
interpreter commands = parse commands 0 0 []

parse :: [String] -> Int -> Int -> [Int] -> [String]
parse [] x y z = map show z 
parse (xx:xs) x y z =  
    case xx of
        "up"  -> parse xs x (y + 1) z
        "down"   -> parse xs x (y - 1) z
        "left"   -> parse xs (x - 1) y z
        "right"     -> parse xs (x + 1) y z
        "printX" -> parse xs x y (z ++ [x])
        "printY" -> parse xs x y (z ++ [y])
        _        -> map show (z : [])
-- :}

-- interpreter ["up","up","up","printY","down","printY"]


