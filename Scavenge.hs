module Scavenge where
import Dictionaries
import Data.List (nub, sort)
import Data.Char
import qualified Data.Map as Map

import qualified Data.List as List


import Debug.Trace
import Data.Map (valid)

--                                          Type Aliases
-- These type aliases help to abstract our code. 
-- 
type Hand = [Char]
type Move = String
type Play = [Move]
type Dictionary = [String]

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the --test flag) even if some functions are left undefined.
--
--                                       Milestone
--

-- Score takes a move and returns the sum of the letter values, according to the Scrabble scoring
-- system: https://hasbro-new.custhelp.com/app/answers/detail/a_id/55/
-- A helper function may be useful.
score :: Move -> Integer
score "" = 0
score x
    | isValidChar (head x) = perLetter (head x) + score (tail x)
    | otherwise = error "Invalid Character input"

-- score "QA" == 11
-- score "JF" == 12


isValidChar :: Char -> Bool
isValidChar b = b `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


perLetter :: Char -> Integer
perLetter x
  | toUpper x    `elem` "AEIOULSTNR" = 1
  | toUpper x    `elem` "DG" = 2
  | toUpper x    `elem` "BCMP" = 3
  | toUpper x    `elem` "FHVWY" = 4
  | toUpper x    `elem` "K" = 5
  | toUpper x    `elem` "JX" = 8
  | toUpper x    `elem` "QZ" = 10
  | otherwise = 0

-- scorePlay takes a play and returns the total score of all words.
scorePlay :: Play -> Integer
scorePlay [] = 0
scorePlay p = score (head p) + scorePlay (tail p)
-- scorePlay ["KB", "QA"] == 19 

-- remove takes an element and a list, and returns the list with one copy of that element removed.
-- You should not assume the list is sorted. If there are multiple copies of the element in the list,
-- only remove one of them. If the element doesn't occur, you should throw an error.
remove :: Eq a => a -> [a] -> [a]
remove item [] = error "List is empty or element is not in list"
remove item (x:xs)
    |item == x = xs
    |otherwise = x : remove item xs
-- remove 7 [7,3,1,7,5] = [3,1,7,5] 
-- The order here doesn't matter, if you remove the second 7 it is okay.

-- updateHand should take a hand (a list of characters), and a move (a string), and return the hand
-- that remains after that move is played.
updateHand :: Hand -> Move -> Hand
updateHand hBefore mv = foldl (flip remove) hBefore mv
{- updateHand hBefore [] = hBefore
updateHand hBefore (mv:rest) = updateHand (remove mv hBefore) rest -}
--updateHand hBefore rest = foldl (flip remove) hBefore rest (VS CODE Produced)
--updateHand hBefore mv = [hAfter | hAfter <- hBefore, hAfter  `notElem` mv ] {- Does not work -}
-- updateHand "HELLO" "LO" = "HEL"

-- canMake takes a hand and a move, and tells you if that move can be made with that hand. Be sure to
-- consider the possibility that a letter may occur more times in the move than it does in the hand.



{- canMake :: Hand -> Move -> Bool
canMake hnd move =
  let handFreq = countFreq hnd
      moveFreq = countFreq move
  in List.foldl' (\acc (c, freq) -> acc && Map.findWithDefault 0 c handFreq >= freq) True (Map.toList moveFreq)
     && List.foldl' (\acc (c, freq) -> acc && Map.findWithDefault 0 c moveFreq <= freq) True (Map.toList handFreq)


--CHATGPT aided. Not sure I understand >>foldl` and why it works. Also even thought this is a faster canMake algorithm, it doesnt work faster for bestplay...
countFreq :: Ord a => [a] -> Map.Map a Int
countFreq = Map.fromListWith (+) . map (\x -> (x, 1))
-} --FASTEST BUT FAILS BESTPLAY
{-canMake :: Hand -> Move -> Bool
canMake hand move =
  let handFreq = countFreq hand
      moveFreq = countFreq move
  in all (\(c, freq) -> Map.findWithDefault 0 c handFreq >= freq) (Map.toList moveFreq)

-} --SOMEHOW FASTER FOR CANMAKE BUT SLOWER FOR BEST PLAY????
canMake :: Hand -> Move -> Bool
canMake hand []  = True
canMake hand (m:mRest) = (m `elem` hand) && canMake (remove m hand) mRest --CURRENT

{-
canMake :: Hand -> Move -> Bool
canMake hand move = all (\c -> length (filter (== c) move) <= length (filter (== c) hand)) move
-}
{- canMake [] move = False
canMake hand [] = True
canMake (h:hRest) (m:mRest) = (h==m && canMake hRest mRest) || canMake(h:hRest) mRest
-}

--First Case accounts for the move being the whole hand, second case accounts for whether or not the move is elsewhere in the hand (not at the start)
-- "DNAHTSET" `canMake` "HAND" = True 
-- "DNAHTSET" `canMake` "HAAND" = False
-- For full credit, this must run in near-linear time (n log n is sufficient)

-- isValidMove tests if a move is valid with respect to a dictionary and hand: 
-- the move must be a word in the dictionary and a move that can be made with the hand.
isValidMove :: Dictionary -> Hand -> Move -> Bool
isValidMove dict (h:hRest) (m:mRest) = canMake (h:hRest) (m:mRest) && m:mRest `elem` dict
-- isValidMove tinyDict "MKEKIOCHAUX" "MAKE" = TRUE
-- isValidMove tinyDict "MKEKIOCHAUX" "MAXKE" = FALSE
-- isValidMove tinyDict "MKEKIOCHAUX" "FAKE" = FALSE

-- isValidPlay checks if a play is valid. Each move in the play must be a word in the dictionary, and
-- must be playable using whatever remains of the hand after all previous moves have been made.
isValidPlay :: Dictionary -> Hand -> Play -> Bool
isValidPlay dict hnd [] = True
isValidPlay dict hnd (p:pRest) =
  let remHand = updateHand hnd p in
  canMake hnd p && p `elem` dict && isValidPlay dict remHand pRest

{- isValidPlay :: Dictionary -> Hand -> Play -> Bool
isValidPlay dict hnd [] = True
isValidPlay dict hnd (p:pRest) =
  let remHand = [ x | x <- hnd, x `notElem` p] in
  canMake hnd p && p `elem` dict && isValidPlay dict remHand pRest
  -}

-- isValidPlay tinyDict "TMAKE" ["TAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["MAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["TAKE","MAKE"] = False

-- validMoves: takes a dictionary and a hand, and returns all words that can be
-- created by letters in the hand. Order does not matter.
validMoves :: Dictionary -> Hand -> [Move]
validMoves dict [] = []
validMoves dict h = [ word | word <- dict, canMake h word ]
-- validMoves shortDict "PEMDOVZIJM" = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]

--                                  End of Milestone!

--                                  Core Project 

-- --- Greedy Algorithm

-- greedyPlay: choose the best move you can at any given point in time, then check to see what
-- other moves you can make.
greedyPlay :: Dictionary -> Hand -> Play
greedyPlay dict h  = if length (validMoves dict h) > 0 then
    let wordslst = validMoves dict h in
    let word = head [word | word <- wordslst, score word == maximum (map score wordslst), score word /= 0] in
     word : greedyPlay dict (updateHand h word)
     else []





-- greedyPlay shortDict "CLOSEFLOOR" = ["FORCE", "SO"] 

-- --- Brute Force Algorithms
-- You are going to search for the best play, instead of just choosing the best move one at a time.
-- To do so, you will consider every possible play, by considering every possible combination of
-- words. You will implement two variants. 

-- powerset: return the powerset of the input, i.e. the list of all sub-lists.
-- You may assume the list has no duplicates. 
-- The output should not have duplicates, up to sorting.
powerset :: [a] -> [[a]]
powerset = foldl (\acc x -> acc ++ map (x:) acc) [[]]
--VS CODE ASSISTED for foldl input and \acc is an accumulator for previous values
{-
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ [x:ys | ys <- powerset xs]
-}
--Non full credi solution above and commented out

-- powerset [1,2] = [[],[1],[1,2],[2]]
-- It is acceptable to have [2,1] INSTEAD of [1,2], but not in addition.
-- length (powerset "abcde") = 32

-- The Naive Brute Force approach (naiveBrutePlay) takes every combination of moves in
-- the dictionary: the powerset of the dictionary. It will only work with very small
-- dictionaries, like tenWords.  You will then choose the best valid play out of that list.
naiveBrutePlay :: Dictionary -> Hand -> Play 
naiveBrutePlay dict hnd = 
  let allPlays = powerset dict in
    if null allPlays
      then error "No valid plays"
      else
        let scoreAllowedPlay = [scorePlay play | play <- allPlays, isValidPlay dict hnd play]
            bestBrutePlay = [play | play <- allPlays, isValidPlay dict hnd play, scorePlay play == maximum scoreAllowedPlay] in
            head bestBrutePlay

        --    bestBrutePlay = head [play | play <- allPlays, scorePlay play == maxScore, isValidPlay dict hnd play] in
        --bestBrutePlay
  
  {- --DOESN'T WORK
  let allPlays = powerset(dict) in
      if null allPlays then error "Empty List" else 
        let maxScore = maximum (map scorePlay allPlays)
            bestBrutePlay = head [play | play <- allPlays, scorePlay play == maxScore, (isValidPlay dict hnd play) ] in
            bestBrutePlay

            -}
    
-- The Smart Brute Force approach realizes that we can restrict the size of the dictionary
-- before we compute the powerset. There are probably MANY moves in the dictionary that we can't
-- create at all! So, first find all the moves that can be made with this hand. Then, take the
-- powerset of that restricted list to create a list of all plays made up of those moves. Then
-- find the best valid play from that list.
smartBrutePlay :: Dictionary -> Hand -> Play
smartBrutePlay dict hnd =
  let allAllowedPlays = powerset (validMoves dict hnd) in
    if null allAllowedPlays
      then error "No Valid Plays"
      else
        let scoreAllowedPlay = [scorePlay play | play <- allAllowedPlays, isValidPlay dict hnd play]
            bestBrutePlay = [play | play <- allAllowedPlays, isValidPlay dict hnd play, scorePlay play == maximum scoreAllowedPlay] in
            head bestBrutePlay




-- --- Best Play Algorithm

-- Finally we will try a recursive strategy to find the best play. Even the smart brute force
-- fails for large hands: I can make a lot of moves, but not a lot of moves simultaniously. Thus
-- the list of all possible plays is large, but the list of all valid plays is still likely
-- small. 
-- For this algorithm, start with the list of valid moves. Then, for each move find the
-- best play for the remaining hand. Select the hand that leads to the best overall score, counting both
-- the score of the move and the score of the best remaining play.

bestPlay :: Dictionary -> Hand -> Play
bestPlay dict hnd =
     let allowedMoves = validMoves dict hnd in
 if null (allowedMoves) then [] else
  let fullListOfPlays = [move : bestPlay dict (updateHand hnd move)|move <- allowedMoves]
      scoredfullListOfPlays = [scorePlay play | play <-fullListOfPlays]
      bestPlayReturn = [play | play<-fullListOfPlays, scorePlay play == maximum scoredfullListOfPlays] in
        head bestPlayReturn
  
