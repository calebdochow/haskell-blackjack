-- Card definitions
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show, Enum)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Show, Enum)
data Card = Card Rank Suit deriving (Eq, Show)

type Deck = [Card]
type Hand = [Card]

-- Convert card to its blackjack value
cardValue :: Card -> Int
cardValue (Card rank _) = case rank of
    Ace   -> 11
    Two   -> 2
    Three -> 3
    Four  -> 4
    Five  -> 5
    Six   -> 6
    Seven -> 7
    Eight -> 8
    Nine  -> 9
    Ten   -> 10
    Jack  -> 10
    Queen -> 10
    King  -> 10

-- Calculate hand value, Aces as 1 or 11
handValue :: Hand -> Int
handValue hand = adjust (sum (map cardValue hand)) (countAces hand)
  where
    countAces = length . filter (\(Card r _) -> r == Ace)
    adjust total 0 = total
    adjust total aces
        | total <= 21 = total
        | otherwise = adjust (total - 10) (aces - 1)

-- Show a hand
showHand :: Hand -> String
showHand = unwords . map showCard
  where
    showCard (Card r s) = show r ++ " of " ++ show s

-- Create 52 card deck
fullDeck :: Deck
fullDeck = [Card r s | s <- [Hearts .. Spades], r <- [Two .. Ace]]

-- Pseudo-random generator
prg :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
prg seed a c m n = take (fromIntegral n) $ iterate (\x -> (a*x + c) `mod` m) seed

-- Shuffle deck
shuffle :: Deck -> Integer -> Deck
shuffle deck seed = shuffleHelper deck (prg seed 1664525 1013904223 4294967296 (toInteger (length deck))) []
  where
    shuffleHelper [] _ acc = acc
    shuffleHelper d [] acc = acc
    shuffleHelper d (r:rs) acc =
        let i = fromInteger (r `mod` toInteger (length d))  -- mod current deck size
            (picked, rest) = pick i d
        in shuffleHelper rest rs (acc ++ [picked])

    pick :: Int -> Deck -> (Card, Deck)
    pick i d = (d !! i, take i d ++ drop (i+1) d)

-- Draw a card
draw :: Deck -> (Card, Deck)
draw (c:cs) = (c, cs)
draw [] = error "Deck is empty"

-- Player turn
playerTurn :: Deck -> Hand -> IO (Hand, Deck)
playerTurn deck hand = do
    putStrLn $ "[Your hand]: " ++ showHand hand ++ " (value: " ++ show (handValue hand) ++ ")"
    if handValue hand >= 21 then return (hand, deck)
    else do
        putStrLn "[hit or stand?]"
        cmd <- getLine
        case cmd of
            "hit" -> do
                let (c, deck') = draw deck
                putStrLn $ "You drew: " ++ showCard c
                playerTurn deck' (hand ++ [c])
            "stand" -> return (hand, deck)
            _ -> do
                putStrLn "Please type 'hit' or 'stand'"
                playerTurn deck hand
  where
    showCard (Card r s) = show r ++ " of " ++ show s

-- Dealer turn
dealerTurn :: Deck -> Hand -> IO (Hand, Deck)
dealerTurn deck hand = do
    putStrLn $ "[Dealer]: " ++ showHand hand ++ " (value: " ++ show (handValue hand) ++ ")"
    if handValue hand >= 17 then return (hand, deck)
    else do
        let (c, deck') = draw deck
        putStrLn $ "[Dealer draws]: " ++ showCard c
        dealerTurn deck' (hand ++ [c])
  where
    showCard (Card r s) = show r ++ " of " ++ show s

-- Determine winner
determineWinner :: Hand -> Hand -> IO ()
determineWinner player dealer
    | playerVal > 21 = putStrLn "Bust! Dealer wins."
    | dealerVal > 21 = putStrLn "Dealer busts! You win!"
    | playerVal > dealerVal = putStrLn "You win!"
    | playerVal < dealerVal = putStrLn "Dealer wins!"
    | otherwise = putStrLn "Push!"
  where
    playerVal = handValue player
    dealerVal = handValue dealer

-- Main game
main :: IO ()
main = do
    -- Read the seed
    seedStr <- readFile "seed.txt"
    let seed = read seedStr :: Integer

    -- Shuffle deck
    let deckShuffled = shuffle fullDeck seed

    -- Initial hands
    let (p1, deck1) = draw deckShuffled
    let (d1, deck2) = draw deck1
    let (p2, deck3) = draw deck2
    let (d2, deck4) = draw deck3

    let playerHand = [p1, p2]
    let dealerHand = [d1, d2]

    -- Show dealer first card
    putStrLn $ "[Dealer]: " ++ showCard d1
    -- Player turn
    (finalPlayerHand, deck5) <- playerTurn deck4 playerHand

    -- Only run dealer turn if player hasn't busted
    if handValue finalPlayerHand > 21
        then putStrLn "Bust! Dealer wins."
        else do
            (finalDealerHand, _) <- dealerTurn deck5 dealerHand
            determineWinner finalPlayerHand finalDealerHand
  where
    showCard (Card r s) = show r ++ " of " ++ show s
