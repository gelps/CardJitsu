-- CPSC 312 Project Fall 2019 - Card Jitsu in Haskell
-- Project #1 by James Beninger, Jeffrey Wang, Roger Wang
module CardJitsu where

import System.IO

-- The current state of the game: [Card] [Card] [Card] [Card] represents the Cards won by the player and computer and cards in the hand of the player and computer respectively

data State = State [Card] [Card] [Card] [Card] Int
        deriving (Show)

-- The cards: String is one of: "FIRE" "ICE" "WATER" and Int is 0 < Int <= 10

data Card = Card String Int
        deriving(Eq, Show)

-- Compare the Cards to determine Winner, first argument should be for the player, 2nd for CPU; 1 = player win 2 = cpu win -1 = tie
-- Should follow the logic WATER > FIRE, FIRE > ICE, ICE > WATER; if both cards are same type highest number wins; if same type same number then tie

determineWinner :: Card -> Card -> Int
determineWinner (Card pTy pVal) (Card cTy cVal) = if pTy == cTy
                                                     then if pVal == cVal then -1 else if pVal > cVal then 1 else 2
                                                     else if ((pTy == "WATER" && cTy == "FIRE") || (pTy == "FIRE" && cTy == "ICE") || (pTy == "ICE" && cTy == "WATER")) then 1 else 2

-- Remove card from Array - idea is to use it in the IO to change game state of cards held by player and CPU

removeCard :: Card -> [Card] -> [Card]
removeCard toRemove set = [x | x<-set, x /= toRemove]

-- Check to see if anyone has won the game, i.e. if player or cpu has one of each "FIRE" "ICE" "WATER" in their set of won cards; 1 = player win 2 = CPU win -1 = still playing

checkWinner :: [Card] -> [Card] -> Int
checkWinner pSet cSet | setContainsAllElements pSet = 1
                      | setContainsAllElements cSet = 2
                      | otherwise                   = (-1)

-- Helper function to Check winner, checks to see if a particular set contains one of each "FIRE" "ICE" "WATER

setContainsAllElements :: [Card] -> Bool
setContainsAllElements set = if length (filter (\ (Card typ val) -> typ == "FIRE") set) > 0
                                then if length (filter (\ (Card typ val) -> typ == "ICE") set) > 0
                                   then if length (filter (\ (Card typ val) -> typ == "WATER") set) > 0 then True else False
                                else False
                             else False

-- Before playing, check if player actually has the card they want to play

hasCard :: Card -> [Card] -> Bool
hasCard card set = elem card set

-- function to let CPU select their card default is to take the first one, may change later, probably not

cpuCard :: [Card] -> Card
cpuCard cardsToPickFrom = head cardsToPickFrom

--- ******************************************* PLAY FUNCTIONALITY - type "go" in ghci to start *************************************

play :: State -> IO State
play (State pWon cWon pCard cCard winner) =
   do
      putStrLn("")
      putStrLn("A New Round is Starting!")
      if (checkWinner pWon cWon == 1) then do
         putStrLn("You won!")
         return (State pWon cWon pCard cCard winner)
      else if (checkWinner pWon cWon == 2) then do
              putStrLn("You lost!")
              return (State pWon cWon pCard cCard winner)
           else do
              putStrLn("Your Cards Won Are:")
              putStrLn(getStringFromCardType pWon)
              putStrLn("Computer Cards Won Are:")
              putStrLn(getStringFromCardType cWon)
              putStrLn("Your Cards Held Are:")
              putStrLn(getStringFromCardType pCard)
              putStrLn("Select a Card to play - valid syntax is f1 ... f10, i1 ... i10, w1 ... w10")
              chosen <- getLine
              let chosenCard = stringToCard chosen
              if (hasCard chosenCard pCard) then do
                 let computerCard = (cpuCard cCard)
                 let newpCard = (removeCard chosenCard pCard)
                 let newcCard = (removeCard computerCard cCard)
                 let winner = determineWinner chosenCard computerCard
                 if (winner == 1) then do
                    putStrLn("You won the round!")
                    putStrLn("Your card " ++ (showCardType chosenCard) ++ " WON against the computer's card " ++ (showCardType computerCard))
                    play (State (chosenCard:pWon) cWon newpCard newcCard winner)
                 else do
                    if (winner == 2) then do
                       putStrLn("You lost the round!")
                       putStrLn("Your card " ++ (showCardType chosenCard) ++ " LOST against the computer's card " ++ (showCardType computerCard))
                       play (State pWon (computerCard:cWon) newpCard newcCard winner)
                    else do
                       putStrLn("You won the round!")
                       putStrLn("Your card " ++ (showCardType chosenCard) ++ " DREW against the computer's card " ++ (showCardType computerCard))
                       play (State pWon cWon newpCard newcCard winner)
              else do
                 putStrLn("You don't have that card!")
                 play (State pWon cWon pCard cCard winner)

go :: IO State
go = play beginState

-- This will return a string of all the cards in an array; for the purposes of using print statements

getStringFromCardType cardArray = foldr (\ fst snd -> fst ++ ", " ++ snd) "" (getArrayCardType cardArray)
getArrayCardType cardArray = [showCardType x | x<-cardArray]
showCardType (Card tp val) = tp ++ " " ++ (toStringInt val)

-- Convert Int to String

toStringInt :: Int -> String
toStringInt 1 = "1"
toStringInt 2 = "2"
toStringInt 3 = "3"
toStringInt 4 = "4"
toStringInt 5 = "5"
toStringInt 6 = "6"
toStringInt 7 = "7"
toStringInt 8 = "8"
toStringInt 9 = "9"
toStringInt 10 = "10"

-- List of Cards:

f1 = Card "FIRE" 1
f2 = Card "FIRE" 2
f3 = Card "FIRE" 3
f4 = Card "FIRE" 4
f5 = Card "FIRE" 5
f6 = Card "FIRE" 6
f7 = Card "FIRE" 7
f8 = Card "FIRE" 8
f9 = Card "FIRE" 9
f10 = Card "FIRE" 10

i1 = Card "ICE" 1
i2 = Card "ICE" 2
i3 = Card "ICE" 3
i4 = Card "ICE" 4
i5 = Card "ICE" 5
i6 = Card "ICE" 6
i7 = Card "ICE" 7
i8 = Card "ICE" 8
i9 = Card "ICE" 9
i10 = Card "ICE" 10

w1 = Card "WATER" 1
w2 = Card "WATER" 2
w3 = Card "WATER" 3
w4 = Card "WATER" 4
w5 = Card "WATER" 5
w6 = Card "WATER" 6
w7 = Card "WATER" 7
w8 = Card "WATER" 8
w9 = Card "WATER" 9
w10 = Card "WATER" 10

-- String Convert Cards -- mostly to be used for player input via string
stringToCard :: String -> Card
stringToCard str | str == "f1" = Card "FIRE" 1
                 | str == "f2" = Card "FIRE" 2
                 | str == "f3" = Card "FIRE" 3
                 | str == "f4" = Card "FIRE" 4
                 | str == "f5" = Card "FIRE" 5
                 | str == "f6" = Card "FIRE" 6
                 | str == "f7" = Card "FIRE" 7
                 | str == "f8" = Card "FIRE" 8
                 | str == "f9" = Card "FIRE" 9
                 | str == "f10" = Card "FIRE" 10
                 | str == "i1" = Card "ICE" 1
                 | str == "i2" = Card "ICE" 2
                 | str == "i3" = Card "ICE" 3
                 | str == "i4" = Card "ICE" 4
                 | str == "i5" = Card "ICE" 5
                 | str == "i6" = Card "ICE" 6
                 | str == "i7" = Card "ICE" 7
                 | str == "i8" = Card "ICE" 8
                 | str == "i9" = Card "ICE" 9
                 | str == "i10" = Card "ICE" 10
                 | str == "w1" = Card "WATER" 1
                 | str == "w2" = Card "WATER" 2
                 | str == "w3" = Card "WATER" 3
                 | str == "w4" = Card "WATER" 4
                 | str == "w5" = Card "WATER" 5
                 | str == "w6" = Card "WATER" 6
                 | str == "w7" = Card "WATER" 7
                 | str == "w8" = Card "WATER" 8
                 | str == "w9" = Card "WATER" 9
                 | otherwise = Card "WATER" 10

-- Player Card Set
pcrd = [f1, f5, f6, f10, i5, i7, i8, i10, w1, w3, w6, w8, w10]

-- CPU Card Pattern
ccrd = [w7, i9, w5, w10, i2, i7, f1, f7, w6, f4, w3, f3, i1, i6, w9]

-- BEGIN STATE
beginState = State [] [] pcrd ccrd (-1)
