# CardJitsu
Conceptual Simple Recreation of Former Club Penguin Minigame "Card Jitsu" in Haskell 
created for CPSC 312 2019 W1 at UBC

The game is played with a set of 30 cards. Each card has an element "FIRE", "WATER" or "ICE" and a number 1-10 representing
its relative strength.

The game is played in rounds until a play wins the game.

How Each Round Works:
Each turn both players will select one card from their deck without knowledge of their opponenets selection.
The two cards will then be revealed at the same time and a winner of the round will be determined.

How the Winner of the Round is Determined:
The primary method of determining a winner is based on the element triangle. 
"FIRE" beats "ICE", "ICE" beats "WATER" and "WATER" beats "FIRE"
If both cards are of the same element, the card's relative strengths will be used to determine the winner, larger the better
In a situation where both cards are the same element and are the same strength (i.e. identical cards),
the round will be declared a draw.

How the Winner of the Game is Determined:
The game will keep track of all the cards that have won a round.
A player wins the game when they have won a round with at least one card from each of the three elements.
