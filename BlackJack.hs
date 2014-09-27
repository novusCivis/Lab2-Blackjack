-- Task A
-- size hand2
--   = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--   = ...
--   = 2

-- size hand 2
-- = size (Add card (Numeric 2) Hearts)(Add (card Jack Spades) Empty))
-- = 1 + size (Add (card Jack Spades)Empty))
-- = 1 + 1 + size(Empty)
-- = 1 + 1 + 0 = 2

-- Task B
module BlackJack where
import Cards
import Wrapper
import Test.QuickCheck
import System.Random

-- My first card, the King of Spades
aCard1 :: Card
aCard1 = Card King Spades

-- My second card, the Ace of Hearts
aCard2 :: Card
aCard2 = Card Ace Hearts

-- aHand defines a hand created to test the functions
aHand :: Hand
aHand = Add aCard1 (Add aCard2 Empty)

-- empty defines an empty hand
empty :: Hand
empty = Empty

-- valueRank calculates the value of a rank
valueRank :: Rank -> Integer
valueRank (Numeric i)   = i
valueRank Ace           = 11
valueRank _             = 10

-- valueCard calculates the value of a specific card
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- numberOfAces checks how many Aces are present in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty      = 0
numberOfAces (Add (Card Ace s) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand

-- valueHand calculates the value of a hand without regarding the dual
-- properties of the Ace
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add card hand) = valueCard card + valueHand hand

-- value checks if the player is bust when counting an Ace as 11. If that
-- is the case it exchanges the value into 1.
value :: Hand -> Integer
value hand | valueHand hand > 21 = valueHand hand - (10 * numberOfAces hand)
           | otherwise = valueHand hand

-- gameOver takes a hand as an argument and calculates if the player is bust
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- winner checks if the guest or the bank wins the game
winner :: Hand -> Hand -> Player
winner guesthand bankhand   | gameOver guesthand = Bank
                            | gameOver bankhand = Guest
                            | value guesthand > value bankhand = Guest
                            | value bankhand > value guesthand = Bank
                            | value guesthand == value bankhand = Bank

-- Task C

-- "<+" takes two hands and puts the first over the second
(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
Add card hand1 <+ hand2 = Add card (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = (size p1) + (size p2) == size (p1 <+ p2)

-- Task D

-- fullSuit returns, when given a suit, the full suit
fullSuit :: Suit -> Hand
fullSuit suit = ((Add (Card (Ace) suit)
                  (Add (Card (King) suit)
                  (Add (Card (Queen) suit)
                  (Add (Card (Jack) suit) (fullSuit' Empty suit 10))))))

-- fullSuit' is a helper function that adds all Numeric cards to the fullSuit
-- without writing each one out
fullSuit' :: Hand -> Suit -> Integer -> Hand
fullSuit' hand suit 1 = hand
fullSuit' hand suit counter = fullSuit' (Add (Card (Numeric counter) suit) hand) suit (counter-1)

-- fullDeck returns a full deck of cards
fullDeck :: Hand
fullDeck = fullSuit Spades <+ fullSuit Hearts <+ fullSuit Diamonds <+ fullSuit Clubs


-- Task E

-- draw takes one hand and one deck and draws one card from the deck.
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: the deck is empty"
draw (Add card hand1) hand2 = (hand1 , Add card hand2)

-- Task F

-- playBank' is a helper function that takes a deck and the banks hand and
-- executes the banks playing in accordance to the rules
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand <= 16 = playBank' deck' bankHand'
                        | otherwise = bankHand
                        where (deck' , bankHand') = draw deck bankHand

-- playBank is the main function that given a deck returns the banks final hand
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- Task G

-- shuffle takes a StdGen and a hand and then returns the hand shuffled
shuffle :: StdGen -> Hand -> Hand
shuffle g deck = shuffle' g deck Empty

shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' _ Empty sHand = sHand
shuffle' g deck sHand = shuffle' g' deck' sHand'
        where (deck', sHand') = pickCard deck sHand n
              (n, g') = randomR (1, size deck) g

-- pickCard solves the problem of moving the n:th card from one hand to the other
pickCard :: Hand -> Hand -> Int -> (Hand, Hand)
pickCard (Add c h) sDeck 1 = (h, Add c sDeck)
pickCard (Add c h) sDeck n = (Add c h', sDeck')
        where (h', sDeck') = pickCard h sDeck (n-1)

-- quickCheck properties for testing the shuffle function
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty    = False
c `belongsTo` Add c' h = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)

-- All test properties tested with quickCheck and passed 100 tests.

-- Task H

-- The final instructed implementation provided on the lab page to tie the knot.

implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

--

main :: IO ()
main = runGame implementation
