-- CPSC 312 Project Fall 2019 - Card Arena in Haskell
-- Project #1 by James Beninger, Jeffrey Wang, Roger Wang
module CardArena where

import System.IO
--import System.Random

-- The current state of the game: [Card] [Card] [Card] [Card] represents the Cards won by the player and computer and cards in the hand of the player and computer respectively,
--                                Bool is true if the current round is still going or false if it has been won

data State = State [Card] [Card] [Card] [Card] Bool
        deriving (Show)

-- Int is the number of wins, [Card] is the cards played by the player last game in order, and Int is the accumulator for different arena types
data Mem = Mem Int [Card] Int

-- Card is a tuple consisting of a String representing its name, an Element, and an Int which is 0 < Int <= 10

data Card = Card String Element Int
        deriving (Show, Eq)

cName :: Card -> String
cName (Card n e v) = n

cElem :: Card -> Element
cElem (Card n e v) = e

changeElem :: Card -> Element -> Card
changeElem (Card n e v) newE = Card n newE v

cVal :: Card -> Int
cVal (Card n e v) = v

changeVal :: Card -> Int -> Card
changeVal (Card n e v) newV = Card n e newV

printCard :: Card -> String
printCard c = (cName c) ++ " of type " ++ show (cElem c) ++ " with size " ++ show (cVal c)

-- Element is an enumeration either of FIRE, ICE, or WATER
data Element = FIRE | ICE | WATER

instance Ord Element where
         WATER > FIRE = True
         FIRE > ICE = True
         ICE > WATER = True
         _ > _ = False
         _ < _ = False
         _ <= _ = False
         _ >= _ = False
         

instance Eq Element where
         WATER == WATER = True
         FIRE == FIRE = True
         ICE == ICE = True
         _ == _ = False

instance Show Element where
         show WATER = "water"
         show FIRE = "fire"
         show ICE = "ice"

-- Compare the Cards to determine Winner, first argument should be for the player, 2nd for CPU; True means the player wins, False means the CPU wins
-- The cards' elements take priority deciding the winner and size is used for ties

roundWinner :: Card -> Card -> Int -> Bool
roundWinner p c acc = if (cElem p) == (cElem c) then p == (roundType acc p c) else (cElem p) > (cElem c)

-- roundType is a helper for the roundWinner function that prevents ties and breaks up the monotony of the gameplay.
-- Returns a function representative of the arena type that takes 2 cards and returns the "winning" card
-- ASSUME: The elements of both cards that will be passed into the returned function are equal

roundType :: Int -> (Card -> Card -> Card)
roundType r = case r of
          1 -> (\p c -> if (cVal p) > (cVal c) then p else c) -- "This arena's suited to the bigger card"
          2 -> (\p c -> if (cVal p) < (cVal c) then p else c) -- "This arena's cramped, the smallest card wins here"
          3 -> (\p c -> if (cVal (changeVal p 10)) > (cVal c) then p else c) -- "This arena's huge, good thing your card took a growth potion!"
          4 -> (\p c -> if (cVal (changeVal p 1)) > (cVal c) then p else c) -- "This arena's huge, your card shouldn't have taken that shrinking potion!"
          5 -> (\p c -> if (cVal (changeVal p 10)) < (cVal c) then p else c) -- "This arena's small, your card shouldn't have taken a growth potion!"
          6 -> (\p c -> if (cVal (changeVal p 1)) < (cVal c) then p else c) -- "This arena's tiny, good thing card took that shrinking potion!"
          7 -> (\p c -> if (cVal p) > (cVal (changeVal c 10)) then c else p) -- "This arena's huge! It's a shame your opponent took a growth potion!"
          8 -> (\p c -> if (cVal p) > (cVal (changeVal c 1)) then c else p) -- "This arena's big, it's a good thing your opponent took a shrinking potion!"
          9 -> (\p c -> if (cVal p) < (cVal (changeVal c 10)) then c else p) -- "This arena's small, it's a good thing your opponent took a growth potion!"
          _ -> (\p c -> if (cVal p) < (cVal (changeVal c 1)) then c else p) -- "This arena's cramped, it's a good thing your opponent took a shrinking potion!"

-- Changes the order of the computer's hand to respond to the cards played by the player last game.
-- ccrd represents all the cards the computer could possibly play, oldGame represents the cards played by the player last game,
-- and the return value is the cards in the order that the computer will play its cards next game

changeHand :: [Card] -> [Card] -> [Card]
changeHand ccrd oldGame = if ((length oldGame) == 0) -- base case
                             then ccrd
                             else (head (take 1 [c | c<-ccrd, ((cElem c) > (cElem (head oldGame)))])) : (changeHand (removeCard ((head (take 1 [c | c<-ccrd, ((cElem c) > (cElem (head oldGame)))]))) ccrd) (tail oldGame))

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
setContainsAllElements set = if (length (filter (\c -> (cElem c) == FIRE) set) > 0) && (length (filter (\c -> (cElem c) == ICE) set) > 0) && (length (filter (\c -> (cElem c) == WATER) set) > 0) then True else False

-- Before playing, check if player actually has the card they want to play

hasCard :: Card -> [Card] -> Bool
hasCard card set = (length [x | x<-set, x /= card]) > 0

-- function to let CPU select their card default is to take the first one, may change later, probably not
-- TODO: Implement the CPU choices to reflect the last game played

cpuCard :: [Card] -> Card
cpuCard cardsToPickFrom = head cardsToPickFrom

--- ******************************************* PLAY FUNCTIONALITY - type "go" in ghci to start *************************************

play :: State -> Mem -> IO State
play (State pWon cWon pCard cCard winner) (Mem wins oldGame acc) =
   do
      putStrLn("")
      putStrLn("A New Round is Starting!")
      if (checkWinner pWon cWon == 1) then do
         putStrLn("Total number of wins: " ++ (show wins))
         putStrLn("You won! Play again? (Y/N)")
         y <- getLine
         if (y == "y" || y == "Y")
            then do
            play (State [] [] pcrd (changeHand ccrd oldGame) False) (Mem (wins + 1) [] 1)
         else do
            return (State pWon cWon pCard cCard winner)
      else if (checkWinner pWon cWon == 2) then do
              putStrLn("Total number of wins: " ++ (show wins))
              putStrLn("You lost! Play again? (Y/N)")
              y <- getLine
              if (y == "y" || y == "Y")
                 then do
                 play (State [] [] pcrd (changeHand ccrd oldGame) False) (Mem (wins + 1) [] 1)
              else do
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
              let chosenCards = [x | x<-pCard, (cName x) == chosen]
              if ((length chosenCards) > 0)
                 then do
                 let chosenCard = head chosenCards
                 let computerCard = (cpuCard cCard)
                 let newpCard = (removeCard chosenCard pCard)
                 let newcCard = (removeCard computerCard cCard)
                 let winner = roundWinner chosenCard computerCard acc
                 if (winner) then do
                   putStrLn("You won the round!")
                   putStrLn("Your card " ++ (cName chosenCard) ++ " WON against the computer's card " ++ (cName computerCard))
                   play (State (chosenCard:pWon) cWon newpCard newcCard winner) (Mem wins (chosenCard : oldGame) (acc + 1))
                 else do
                   putStrLn("You lost the round!")
                   putStrLn("Your card " ++ (cName chosenCard) ++ " LOST against the computer's card " ++ (cName computerCard))
                   play (State pWon (computerCard:cWon) newpCard newcCard winner) (Mem wins (chosenCard : oldGame) (acc + 1))
              else do
                 putStrLn("You don't have that card!")
                 play (State pWon cWon pCard cCard winner) (Mem wins oldGame acc)

go :: IO State
go = play beginState beginMem

-- This will return a string of all the cards in an array; for the purposes of using print statements

getStringFromCardType :: [Card] -> String
getStringFromCardType cardArray = foldr (\ h t -> h ++ "\n" ++ t) "" [printCard x | x<-cardArray]

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

f1 = Card "f1" FIRE 1
f2 = Card "f2" FIRE 2
f3 = Card "f3" FIRE 3
f4 = Card "f4" FIRE 4
f5 = Card "f5" FIRE 5
f6 = Card "f6" FIRE 6
f7 = Card "f7" FIRE 7
f8 = Card "f8" FIRE 8
f9 = Card "f9" FIRE 9
f10 = Card "f10" FIRE 10

i1 = Card "i1" ICE 1
i2 = Card "i2" ICE 2
i3 = Card "i3" ICE 3
i4 = Card "i4" ICE 4
i5 = Card "i5" ICE 5
i6 = Card "i6" ICE 6
i7 = Card "i7" ICE 7
i8 = Card "i8" ICE 8
i9 = Card "i9" ICE 9
i10 = Card "i10" ICE 10

w1 = Card "w1" WATER 1
w2 = Card "w2" WATER 2
w3 = Card "w3" WATER 3
w4 = Card "w4" WATER 4
w5 = Card "w5" WATER 5
w6 = Card "w6" WATER 6
w7 = Card "w7" WATER 7
w8 = Card "w8" WATER 8
w9 = Card "w9" WATER 9
w10 = Card "w10" WATER 10

-- Player Card Set
pcrd = [f1, f5, f6, f10, i5, i7, i8, i10, w1, w3, w6, w8, w10]

-- CPU Card Pattern
ccrd = [w7, i9, w5, w10, i2, i7, f1, f7, w6, f4, w3, f3, i1, i6, w9]

-- BEGIN STATE
beginState = State [] [] pcrd ccrd False
beginMem = Mem 0 [] 1