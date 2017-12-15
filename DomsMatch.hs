module DomsMatch where
 import System.Random
 import Data.List
 import Debug.Trace
 import Data.Maybe
 import Data.Function
  
 {-
  Smart player, tries all available tactics regarding if board is empty or not.
 -}
 smartPlayer :: DomsPlayer
 smartPlayer hand InitBoard player scores = tryTactics [winIt, blockEnemyWin, put54, getTo59, putSafeHighest, putHighestScoring] hand InitBoard player scores
 smartPlayer hand board@(Board d1 d2 history) player scores  = tryTactics [winIt, blockEnemyWin, getTo59, putSafeHighest, putHighestScoring] hand board player scores

 {-
  Simple player which will always put highest scoring domino or try to win the game if possible.
 -}
 simplePlayer :: DomsPlayer
 simplePlayer hand InitBoard player scores = tryTactics [winIt, putHighestScoring] hand InitBoard player scores
 simplePlayer hand board@(Board d1 d2 history) player scores  = tryTactics [winIt, putHighestScoring] hand board player scores

 {-
  Safe player which will try to win, if he can't then will try to put a safe domino, if he can't then put any domino.
 -}
 safePlayer :: DomsPlayer
 safePlayer hand InitBoard player scores = tryTactics [winIt, putSafeHighest, putAny] hand InitBoard player scores
 safePlayer hand board@(Board d1 d2 history) player scores  = tryTactics [winIt, putSafeHighest, putAny] hand board player scores

 {-
  Smart player without blockEnemyWin tactic.
 -}
 smartPlayerNoBlock :: DomsPlayer
 smartPlayerNoBlock hand InitBoard player scores = tryTactics [winIt, put54, getTo59, putSafeHighest, playADouble, putHighestScoring] hand InitBoard player scores
 smartPlayerNoBlock hand board@(Board d1 d2 history) player scores  = tryTactics [winIt, getTo59, putSafeHighest, playADouble, putHighestScoring] hand board player scores

 {-
  Framework for trying tactics, will try all the tactics in order they are given.
  Args: Tactics as a list will start executing from head and move to tail,
        my hand, board, my player, scores
  Return: What domino to put at what end
 -}
 tryTactics :: [Tactic] -> Hand -> DomBoard -> Player -> Scores -> (Dom, End)
 tryTactics tactics@(h:t) hand board player scores
  | isntNothing(h hand board player scores) = fromJust (h hand board player scores) -- If tactic doesn't return Nothing then return it.
  | otherwise = (tryTactics t hand board player scores) -- otherwise recursive call to try next tactic
 
 {-
  Tactic type.
  Args: My hand, board, player, scores
  Return: Maybe (Dom, End) if tactic can't be used then Nothing
          (Dom, End) if tactic can be used
 -}
 type Tactic = Hand -> DomBoard -> Player -> Scores -> Maybe (Dom, End)
 
 -- Tactic to win a game (score becomes 61) if possible
 winIt :: Tactic
 winIt hand board player scores = getToNumber hand board player scores 61
 
 -- Tactic to make score for the player 59
 getTo59 :: Tactic
 getTo59 hand board player scores = getToNumber hand board player scores 59
 
 {-
  Tries to get a score to a certain number.
  Args: My hand, board, my player, scores, number of score you want to get
 -}
 getToNumber :: Hand -> DomBoard -> Player -> Scores -> Int -> Maybe (Dom, End)
 getToNumber [] _ _ _ _ = Nothing
 getToNumber hand@(h:t) board player scores@(p1s,p2s) number
  | not(isNothing (playDom player h R board)) && player == P1 && p1s + scoreboard(fromJust (playDom player h R board)) == number = Just (h, R)
  | not(isNothing (playDom player h L board)) && player == P1 && p1s + scoreboard(fromJust (playDom player h L board)) == number = Just (h, L)
  | not(isNothing (playDom player h R board)) && player == P2 && p2s + scoreboard(fromJust (playDom player h R board)) == number = Just (h, R)
  | not(isNothing (playDom player h L board)) && player == P2 && p2s + scoreboard(fromJust (playDom player h L board)) == number = Just (h, L)
  | otherwise = getToNumber t board player scores number
 
 -- Tactic to put (5,4) dom if possible.
 put54 :: Tactic
 put54 [] _ _ _ = Nothing
 put54 hand board player scores
  | doIHaveThisDomino (5,4) hand = Just ((5,4), L)
  | otherwise = Nothing
 
 putAny :: Tactic
 putAny [] _ _ _ = Nothing
 putAny hand board player scores = Just (randomPlayer hand board player scores)
 
 -- Tactic to put most scoring domino
 putHighestScoring :: Tactic
 putHighestScoring hand board player scores = Just (hsdPlayer hand board player scores)
 
 -- Tactic to put highest scoring domino if enemy definately can't score higher.
 putSafeHighest :: Tactic
 putSafeHighest [] _ _ _ = Nothing
 putSafeHighest hand@(h:t) board player scores 
  | True = (putSafeHighestHelp hand board player (getPossibleEnemyDominoes hand board player) scores)
  | otherwise = Nothing
  
 putSafeHighestHelp :: Hand -> DomBoard -> Player -> Hand -> Scores -> Maybe (Dom, End)
 putSafeHighestHelp [] _ _ _ _ = Nothing
 putSafeHighestHelp myHand board player enHand scores
  | maxScorer == ((-1,-1),R) || maxScorer == ((-1,-1),L) = Nothing -- if hsdPlayer can't put anything return Nothing
  |(not(isNothing(playDom player dom end board))) && scoreboard(fromJust(playDom player dom end board))  > (myMaximum(possibleEnemyScores))= (Just maxScorer)
  | otherwise =  (putSafeHighestHelp (removeDomFromHand dom myHand) board player enHand scores)
    where maxScorer@(dom,end) = (hsdPlayer myHand board player scores)
          possibleEnemyScores = (getScorePossibilities enHand (fromJust (playDom player dom end board)) player)
  
 -- My maximum which handles empty list(returns 0)
 myMaximum :: [Int] -> Int
 myMaximum ints 
  | ints == [] = 0
  | otherwise = maximum ints
  
 -- Removes domino from a hand
 removeDomFromHand :: Dom -> Hand -> Hand
 removeDomFromHand _ [] = []
 removeDomFromHand dom hand@(h:t)
  | not(isSameDom dom h) = h : removeDomFromHand dom t
  | otherwise = removeDomFromHand dom t

 -- Tactic to play a double
 playADouble :: Tactic
 playADouble [] _ _ _ = Nothing
 playADouble hand@(h:t) board player scores
  | max >= 3 && doIHaveADoubleOfThat hand whichOneToPlay && not(isNothing(playDom player doubleDom L board)) && differenceInScoreWithHsd hand board player scores doubleDom L < 2 && not (couldEnemyHaveThatDotValueDom whichOneToPlay hand board player) = Just (doubleDom, L)
  | (max) >= 3 && (not(isNothing(playDom player doubleDom R board))) && (differenceInScoreWithHsd hand board player scores doubleDom R) < 2 && (not (couldEnemyHaveThatDotValueDom whichOneToPlay hand board player))= Just (doubleDom, R)
  | otherwise = Nothing
    where whichOneToPlay =  chooseWhichDotValueToPlay (countHowManyOfSameDots hand) (max) 0
          max = maximum (countHowManyOfSameDots hand)
          doubleDom = (whichOneToPlay, whichOneToPlay)
          
 -- Checks if hand has a double of given int
 doIHaveADoubleOfThat :: Hand -> Int -> Bool
 doIHaveADoubleOfThat [] _ = False
 doIHaveADoubleOfThat hand@(h@(d1,d2):t) numDots
  | d1 == numDots && d2 == numDots = True
  | otherwise = doIHaveADoubleOfThat t numDots
 
 -- Counts list, maximum value of it, which one is max, start index.
 chooseWhichDotValueToPlay :: [Int] -> Int -> Int -> Int
 chooseWhichDotValueToPlay [] _ _ = 0
 chooseWhichDotValueToPlay counts@(h:t) maximum start
  | h == maximum = start
  | otherwise = chooseWhichDotValueToPlay t maximum (start + 1)

 {-
  Calculates score difference between most scoring and given domino
  Args: my hand, board, my player, scores, domino, end
  Return: Difference between hsd score and given dom+end score
 -} 
 differenceInScoreWithHsd :: Hand -> DomBoard -> Player -> Scores -> Dom -> End -> Int
 differenceInScoreWithHsd hand@(h:t) board player scores domino end 
  | isntNothing(playDom player hsdDom hsdEnd board) = scoreboard(fromJust(playDom player hsdDom hsdEnd board)) - scoreboard(fromJust(playDom player domino end board))
  | otherwise = 99
    where hsdDom = fst (hsdPlayer hand board player scores)
          hsdEnd = snd (hsdPlayer hand board player scores)
 
 {-
  Checks if enemy can have given dot value in his hand
  Args: dot value, my hand, board, my player
  Return: True if enemy can have that dot value,
          False otherwise
 -}
 couldEnemyHaveThatDotValueDom :: Int -> Hand -> DomBoard -> Player -> Bool
 couldEnemyHaveThatDotValueDom dots hand board player = couldEnemyHaveThatDotValueDomHelp dots hand board player (getPossibleEnemyDominoes hand board player)
 
 couldEnemyHaveThatDotValueDomHelp :: Int -> Hand -> DomBoard -> Player -> Hand -> Bool
 couldEnemyHaveThatDotValueDomHelp _ _ _ _ [] = False
 couldEnemyHaveThatDotValueDomHelp dots hand board player enemyHand@(h:t)
  | domHasDots h [dots] = True
  | otherwise = couldEnemyHaveThatDotValueDomHelp dots hand board player t
  
 {-
  Counts how many of same dot values there are in the hand.
  Args: Hand
  Return: List of Ints which represent how many of that dot value there are in the hand
 -}
 countHowManyOfSameDots :: Hand -> [Int]
 countHowManyOfSameDots [] = []
 countHowManyOfSameDots hand@(h@(d1,d2):t) = [countHowManyDotValues hand 0, countHowManyDotValues hand 1, countHowManyDotValues hand 2, countHowManyDotValues hand 3, countHowManyDotValues hand 4, countHowManyDotValues hand 5, countHowManyDotValues hand 6]
  
 countHowManyDotValues :: Hand -> Int -> Int
 countHowManyDotValues [] _ = 0
 countHowManyDotValues hand@(h@(d1,d2):t) number
  | d1 == number || d2 == number = 1 + countHowManyDotValues t number
  | otherwise = countHowManyDotValues t number

 {-
  Gets score possiblities which can be scored with given hand on given board.
  Args: my hand, board, my player
  Return: List of possible scores as Ints
 -}
 getScorePossibilities :: Hand -> DomBoard -> Player -> [Int]
 getScorePossibilities hand board player = (getScorePossibilitiesR hand board player ++ getScorePossibilitiesL hand board player)
  
 {-
  Gets possible scores for putting dominoes on the RIGHT end.
  Args: My hand, board, my player
  Return: List of ints, every int is score possibility
 -}
 getScorePossibilitiesR :: Hand -> DomBoard -> Player -> [Int]
 getScorePossibilitiesR [] _ _ = []
 getScorePossibilitiesR hand@(h:t) domBoard player
  | not (isNothing (playDom player h R domBoard)) = scoreboard(fromJust(playDom player h R domBoard)) : (getScorePossibilitiesR t domBoard player)
  | otherwise = getScorePossibilitiesR t domBoard player
 
 {-
  Gets possible scores for putting dominoes on the LEFT end.
  Args: My hand, board, my player
  Return: List of ints, every int is score possibility
 -}
 getScorePossibilitiesL :: Hand -> DomBoard -> Player -> [Int]
 getScorePossibilitiesL [] _ _ = []
 getScorePossibilitiesL hand@(h:t) domBoard player
  | not (isNothing (playDom player h L domBoard)) = scoreboard(fromJust(playDom player h L domBoard)) : getScorePossibilitiesL t domBoard player
  | otherwise = getScorePossibilitiesL t domBoard player
 
 {-
  Gets possible enemy dominoes.
  Args: my hand, board, my player
  Return: Enemy hand
 -}
 getPossibleEnemyDominoes :: Hand -> DomBoard -> Player -> Hand
 getPossibleEnemyDominoes hand InitBoard player = (getAllDomsIDontHave hand domSet)
 getPossibleEnemyDominoes hand@(h:t) board@(Board d1 d2 history) player
  | not(isNothing(findDomsEnemyDoesntHave board player)) = ( (removeDomsWhichAreOnTheBoard (removeFromHandWhatEnemyDoesntHave (getAllDomsIDontHave hand domSet) (fromJust (findDomsEnemyDoesntHave board player))) board))
  | otherwise = (removeDomsWhichAreOnTheBoard (getAllDomsIDontHave hand domSet) board)
  
 {-
  Removes dominoes which are on the board.
  Args: Hand, board
  Return: Returns hand which has dominoes removed
 -}
 removeDomsWhichAreOnTheBoard :: Hand -> DomBoard -> Hand
 removeDomsWhichAreOnTheBoard [] _ = []
 removeDomsWhichAreOnTheBoard hand@(h:t) board@(Board d1 d2 history)
  | not(domExistsInHistory h history) = h : removeDomsWhichAreOnTheBoard t board
  | otherwise = removeDomsWhichAreOnTheBoard t board
    
 {-
  Checks if given domino exists in history.
  Args: Domino, History
  Return: True if it exists,
          False if it doesn't
 -}
 domExistsInHistory :: Dom -> History -> Bool
 domExistsInHistory _ [] = False
 domExistsInHistory domino history@(h@(dom, player, moveNum):t)
  | isSameDom dom domino = True
  | otherwise = domExistsInHistory domino t
 
 {-
  Removes from enemy hand what is sure that he doesnt have
  Args: Enemy hand, Dot values what enemy doesn't have
  Return: Enemy hand with removed doms which have dom values
 -}
 removeFromHandWhatEnemyDoesntHave:: Hand -> [Int] -> Hand
 removeFromHandWhatEnemyDoesntHave [] _ = []
 removeFromHandWhatEnemyDoesntHave hand@(handH:handT) dots
  | not (domHasDots handH dots) = handH : removeFromHandWhatEnemyDoesntHave handT dots
  | otherwise = removeFromHandWhatEnemyDoesntHave handT dots
   
 {-
  Checks if domino has any of given dot values
  Args: Domino, dot values as a list of Ints
  Return: True if there exists any of the given dot values,
          False otherwise
 -}
 domHasDots :: Dom -> [Int] -> Bool
 domHasDots _ [] = False
 domHasDots dom@(d1,d2) dots@(h:t)
  | d1 == h || d2 == h = True
  | otherwise = domHasDots dom t
 
 {-
  Gets all the dominoes which I don't have.
  Args: My hand, all dominoes(domSet)
  Return: Hand which is my hand's dominoes removed from all dominoes
 -}
 getAllDomsIDontHave :: Hand -> Hand -> Hand
 getAllDomsIDontHave _ [] = []
 getAllDomsIDontHave [] _ = []
 getAllDomsIDontHave hand@(h@(d1,d2):t) allDoms@(head:tail)
  | not (doIHaveThisDomino head hand) = head : getAllDomsIDontHave hand tail
  | otherwise = getAllDomsIDontHave hand tail
 
 {-
  Gets all the dominoes which I don't have.
  Args: My hand, all dominoes(domSet)
  Return: Hand which is my hand's dominoes removed from all dominoes
 -}
 whatDominosIDontHave :: Hand -> Hand -> Hand
 whatDominosIDontHave _ [] = []
 whatDominosIDontHave [] _ = []
 whatDominosIDontHave hand@(h@(d1,d2):t) allDoms@(head:tail)
  | not (doIHaveThisDomino head hand) = head : whatDominosIDontHave hand tail
  | otherwise = whatDominosIDontHave hand tail
  
 {-
  Find dominoes what enemy doesn't have.
  Args: Board, my player
  Return: List of Ints which represent what dot values enemy doesn't have
 -}
 findDomsEnemyDoesntHave :: DomBoard -> Player -> Maybe [Int]
 findDomsEnemyDoesntHave board@(Board d1 d2 history) player
  | not(isNothing(findWhatEnemyWasKnockingOn history player)) = Just (f board player (fromJust(findWhatEnemyWasKnockingOn history player)))
  | otherwise = Nothing
 
 -- Board, moveNumsWhereKnocks, all doms which he doesnt have
 f :: DomBoard -> Player -> [Int] -> [Int]
 f _ _ [] = []
 f board@(Board d1 d2 history) player moveNums@(h:t)
  | not(isNothing(findWhatEnemyWasKnockingOn history player)) = (fst(head(constructBoardOnMoveNum history h)) : [snd(last(constructBoardOnMoveNum history h))]) ++ f board player t
  | otherwise = f board player t
 
 {-
  Constructs a board on some move number.
  Args: History, move number
  Return: A list of dominoes. Head of the list is leftmost in the board, rightmost for the last.
 -}
 constructBoardOnMoveNum :: History -> Int -> [Dom]
 constructBoardOnMoveNum [] _ = []
 constructBoardOnMoveNum history@(h@(dom, play, moveN):t) moveNum
  | moveN <= moveNum = dom : (constructBoardOnMoveNum t moveNum)
  | otherwise = constructBoardOnMoveNum t moveNum
   
  {-
    Finds move numbers on which enemy was knocking on.
    Args: History, my player
    Return: A list of Ints which represent move numbers on which enemy was knocking.
  -}
 findWhatEnemyWasKnockingOn :: History -> Player ->Maybe [Int]
 findWhatEnemyWasKnockingOn history player
  | fromJust (findMoveNum (sortBy (compare `on` (trd3)) history) player P2) == [] ||  fromJust (findMoveNum (sortBy (compare `on` (trd3)) history) player P1)== [] = Nothing
  | player == P1 =  (findMoveNum (sortBy (compare `on` (trd3)) history) player P2)
  | player == P2 =  (findMoveNum (sortBy (compare `on` (trd3)) history) player P1)

  -- Gets move number when player was knocking. History, currentPlayer, Enemy Player
 {-
  Gets moves numbers on when enemy player was knocking.
  Args: History, my player, enemy player
  Return: A list of ints which represent which moves was enemy knocking on.
 -}
 findMoveNum :: History -> Player -> Player ->Maybe [Int]
 findMoveNum [] _ _ = Just []
 findMoveNum history@(h@(dom, play, moveNum):t) myPlayer prevPlayer
  | play == prevPlayer && play == myPlayer && not(isNothing((findMoveNum t myPlayer play)))= Just ((moveNum - 1) : fromJust (findMoveNum t myPlayer play))
  | otherwise = findMoveNum t myPlayer play

 {-
  Tactic to block enemy from winning. 
 -}
 blockEnemyWin :: Tactic
 blockEnemyWin hand board player scores
  | player == P1 && winIt enemyHand (fromJust(playDom player dom end board)) P2 scores /= Nothing = makeEnemyKnock hand board player
  | player == P2 && winIt enemyHand (fromJust(playDom player dom end board)) P1 scores /= Nothing = makeEnemyKnock hand board player
  | otherwise = Nothing
    where enemyHand = (getPossibleEnemyDominoes hand board player)
          myMove@(dom, end) = (smartPlayerNoBlock hand board player scores)
 
 {-
  Makes enemy knock according to what he has.
  Args: Hand(my hand), Board, Player(my player)
  Returns: Just (Dom,End) or Nothing (if enemy can't be knocked)
 -} 
 makeEnemyKnock :: Hand -> DomBoard -> Player -> Maybe (Dom, End)
 makeEnemyKnock [] _ _ = Nothing
 makeEnemyKnock hand@(h:t) board player 
  | isntNothing(createdBoardL) && (whatDotsEnemyDoesntHaveList!!(fst ldomL)) && (whatDotsEnemyDoesntHaveList!!(snd ldomR)) = Just (h,L)
  | isntNothing(createdBoardR) && whatDotsEnemyDoesntHaveList!!(fst rdomL) && whatDotsEnemyDoesntHaveList!!(snd rdomR) = Just (h,R)
  | otherwise = makeEnemyKnock t board player
    where whatDotsEnemyDoesntHaveList = (whatDotsHandDoesntHave (getPossibleEnemyDominoes hand board player))
          createdBoardL = playDom player h L board
          createdBoardR =(playDom player h R board)
          fromJustedCreatedBoardL@(Board ldomL ldomR lh) = fromJust(createdBoardL)
          fromJustedCreatedBoardR@(Board rdomL rdomR rh) = (fromJust(createdBoardR))  
  
 {-
  Finds what dot values doesn't exist in a hand.
  Args: Hand
  Returns: List of length 6 of bool values.
  e.g if List!!3 is True means there is NOT dot value 3
 -}
 whatDotsHandDoesntHave :: Hand -> [Bool]
 whatDotsHandDoesntHave hand = [not(isDotValueInHand hand 0), not(isDotValueInHand hand 1), not(isDotValueInHand hand 2),
                                not(isDotValueInHand hand 3), not(isDotValueInHand hand 4), not(isDotValueInHand hand 5),
                                not(isDotValueInHand hand 6)]
 {-
  Checks if some dot value exists in the hand.
  Args: Hand, dot value
  Returns: True if there is a domino in the hand
  which has given dot value.
           False otherwise
 -}
 isDotValueInHand :: Hand -> Int -> Bool
 isDotValueInHand [] _ = False
 isDotValueInHand hand@(h@(d1,d2):t) dotValue
  | d1 == dotValue || d2 == dotValue = True
  | otherwise = isDotValueInHand t dotValue
   
 {-
  Checks if domino exists in the hand.
  Args: Domino, Hand
  Returns: True if domino exists in the hand,
           False otherwise 
 -}
 doIHaveThisDomino :: Dom -> Hand -> Bool
 doIHaveThisDomino dom [] = False
 doIHaveThisDomino dom@(d1, d2) hand@(h:t)
  | isSameDom dom h = True
  | otherwise = doIHaveThisDomino dom t
  
 {- 
 Checks if 2 dominoes are the same. 
 Args: Domino1, Domino2. 
 Returns: True if they are the same, false otherwise
 -}
 isSameDom :: Dom -> Dom -> Bool
 isSameDom dom1@(d1,d2) dom2@(d11,d22)
  | dom1 == dom2 = True
  | d1 == d22 && d2 == d11 = True
  | otherwise = False
 
 -- Functions to work on triples
 fst3 :: (a,b,c) -> a
 fst3 (a,_,_) = a
 
 snd3 :: (a,b,c) -> b
 snd3 (_,b,_) = b
 
 trd3 :: (a,b,c) -> c
 trd3 (_,_,c) = c
 
 -- Checks if maybe type isn't nothing
 isntNothing :: Maybe a -> Bool
 isntNothing a
  | not(isNothing a) = True
  | otherwise = False

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 type Dom = (Int,Int)
 -- with highest pip first i.e. (6,1) not (1,6)

 data DomBoard = InitBoard|Board Dom Dom History
                    deriving (Show)
 
 type History = [(Dom,Player,MoveNum)]
 -- History allows course of game to be reconstructed                                            
                                               
 data Player = P1|P2 -- player 1 or player 2
                  deriving (Eq,Show)
 
 data End = L|R -- left end or right end
                  deriving (Eq,Show)
 
 type MoveNum = Int

 type Hand = [Dom]
  
 -- the full set of Doms
 domSet :: [Dom]
 
 domSet = [(6,6),(6,5),(6,4),(6,3),(6,2),(6,1),(6,0),
                 (5,5),(5,4),(5,3),(5,2),(5,1),(5,0),
                       (4,4),(4,3),(4,2),(4,1),(4,0),
                             (3,3),(3,2),(3,1),(3,0),
                                   (2,2),(2,1),(2,0),
                                         (1,1),(1,0),
                                               (0,0)]
                                                                                         
 
 type Move = (Dom,End)
 type Scores = (Int,Int)
                                                                                              
 -- state in a game - p1's hand, p2's hand, player to drop, current board, scores 
 type GameState =(Hand,Hand,Player, DomBoard, Scores)
 
 
 ------------------------------------------------------
 {- DomsPlayer
    given a Hand, the Board, which Player this is and the current Scores
    returns a Dom and an End
    only called when player is not knocking
    made this a type, so different players can be created
 -}
 
 type DomsPlayer = Hand->DomBoard->Player->Scores->(Dom,End)
 
 {- variables
     hand h
     board b
     player p
     scores s
 -}

 -- example players
 -- randomPlayer plays the first legal dom it can, even if it goes bust
 randomPlayer :: DomsPlayer
 
 randomPlayer h b p s 
  |not(null ldrops) = ((head ldrops),L)
  |otherwise = ((head rdrops),R)
  where
   ldrops = leftdrops h b
   rdrops = rightdrops h b
   
 -- hsdplayer plays highest scoring dom
 -- we have  hsd :: Hand->DomBoard->(Dom,End,Int)
 
 hsdPlayer h b p s = (d,e)
                     where (d,e,_)=hsd h b
                     
  -- highest scoring dom

 hsd :: Hand->DomBoard->(Dom,End,Int)
 
 hsd h InitBoard = (md,L,ms)
  where
   dscores = zip h (map (\ (d1,d2)->score53 (d1+d2)) h)
   (md,ms) = maximumBy (comparing snd) dscores
   
 
 hsd h b = 
   let
    ld=  leftdrops h b
    rd = rightdrops h b
    lscores = zip ld (map (\d->(scoreDom d L b)) ld) -- [(Dom, score)]
    rscores = zip rd (map (\d->(scoreDom d R b)) rd)
    (lb,ls) = if (not(null lscores)) then (maximumBy (comparing snd) lscores) else ((-1,-1),-1) -- can't be chosen
    (rb,rs) = if (not(null rscores)) then (maximumBy (comparing snd) rscores) else ((-1,-1),-1)
   in
    if (ls>rs) then (lb,L,ls) else (rb,R,rs)
 
 
                                               
 -----------------------------------------------------------------------------------------
 {- top level fn
    args: 2 players (p1, p2), number of games (n), random number seed (seed)
    returns: number of games won by player 1 & player 2
    calls playDomsGames giving it n, initial score in games and random no gen
 -} 
 
 domsMatch :: DomsPlayer->DomsPlayer->Int->Int->(Int,Int)
 
 domsMatch p1 p2 n seed = playDomsGames p1 p2 n (0,0) (mkStdGen seed)
 
 -----------------------------------------------------------------------------------------
 
{- playDomsGames plays n games

  p1,p2 players
  (s1,s2) their scores
  gen random generator
-}
 
 playDomsGames :: DomsPlayer->DomsPlayer->Int->(Int,Int)->StdGen->(Int,Int)
 
 playDomsGames _ _ 0 score_in_games _ = score_in_games -- stop when n games played
 
 playDomsGames p1 p2 n (s1,s2) gen 
   |gameres==P1 = playDomsGames p1 p2 (n-1) (s1+1,s2) gen2 -- p1 won
   |otherwise = playDomsGames p1 p2 (n-1) (s1,s2+1) gen2 -- p2 won
  where
   (gen1,gen2)=split gen -- get 2 generators, so doms can be reshuffled for next hand
   gameres = playDomsGame p1 p2 (if (odd n) then P1 else P2) (0,0) gen1 -- play next game p1 drops if n odd else p2
 
 -----------------------------------------------------------------------------------------
 -- playDomsGame plays a single game - 61 up
 -- returns winner - P1 or P2
 -- the Bool pdrop is true if it's p1 to drop
 -- pdrop alternates between games
 
 playDomsGame :: DomsPlayer->DomsPlayer->Player->(Int,Int)->StdGen->Player
 
 playDomsGame p1 p2 pdrop scores gen 
  |s1==61 = P1
  |s2==61 = P2
  |otherwise = playDomsGame p1 p2 (if (pdrop==P1) then P2 else P1) (s1,s2) gen2
  where
   (gen1,gen2)=split gen
   (s1,s2)=playDomsHand p1 p2 pdrop scores gen1  
  
 -----------------------------------------------------------------------------------------
 -- play a single hand
  
 playDomsHand :: DomsPlayer->DomsPlayer->Player->(Int,Int)->StdGen->(Int,Int)
 
 playDomsHand p1 p2 nextplayer scores gen = 
   playDoms p1 p2 init_gamestate
  where
   spack = shuffleDoms gen
   p1_hand = take 9 spack
   p2_hand = take 9 (drop 9 spack)
   init_gamestate = (p1_hand,p2_hand,nextplayer,InitBoard,scores) 
   
 ------------------------------------------------------------------------------------------   
 -- shuffle 
 
 shuffleDoms :: StdGen -> [Dom]

 shuffleDoms gen =  
  let
    weights = take 28 (randoms gen :: [Int])
    dset = (map fst (sortBy  
               (\ (_,w1)(_,w2)  -> (compare w1 w2)) 
               (zip domSet weights)))
  in
   dset
   
 ------------------------------------------------------------------------------------------
 -- playDoms runs the hand
 -- returns scores at the end

 
 playDoms :: DomsPlayer->DomsPlayer->GameState->(Int,Int)
 
 playDoms _ _ (_,_,_,_, (61,s2)) = (61,s2) --p1 has won the game
 playDoms _ _ (_,_,_,_, (s1,61)) = (s1,61) --p2 has won the game
 
 
 playDoms p1 p2 gs@(h1,h2,nextplayer,b,scores)
  |(kp1 &&  kp2) = scores -- both players knocking, end of the hand
  |((nextplayer==P1) && (not kp1)) =  playDoms p1 p2 (p1play p1 gs) -- p1 plays, returning new gameState. p2 to go next
  |(nextplayer==P1) = playDoms p1 p2 (p2play p2 gs) -- p1 knocking so p2 plays
  |(not kp2) = playDoms p1 p2 (p2play p2 gs) -- p2 plays
  |otherwise = playDoms p1 p2 (p1play p1 gs) -- p2 knocking so p1 plays
  where
   kp1 = knocking h1 b -- true if p1 knocking
   kp2 = knocking h2 b -- true if p2 knocking
   
 ------------------------------------------------------------------------------------------
 -- is a player knocking?

 knocking :: Hand->DomBoard->Bool
 
 knocking h b = 
  ((null (leftdrops h b)) && (null (rightdrops h b))) -- leftdrops & rightdrops in doms.hs
 
 ------------------------------------------------------------------------------------------
   
 -- player p1 to drop
 
 p1play :: DomsPlayer->GameState->GameState
 
 p1play p1 (h1,h2,_,b, (s1,s2)) = 
  ((delete dom h1), h2, P2,(updateBoard dom end P1 b), (ns1, s2))
   where
    (dom,end) = p1 h1 b P1 (s1,s2)-- call the player, returning dom dropped and end it's dropped at.
    score = s1+ (scoreDom dom end b) -- what it scored
    ns1 = if (score >61) then s1 else score -- check for going bust
    
 
 -- p2 to drop
   
 p2play :: DomsPlayer->GameState->GameState
 
 p2play p2 (h1,h2,_,b,(s1,s2)) = 
  (h1, (delete dom h2),P1, (updateBoard dom end P2 b), (s1, ns2))
  where
   (dom,end) = p2 h2 b P2 (s1,s2)-- call the player, returning dom dropped and end it's dropped at.
   score = s2+ (scoreDom dom end b) -- what it scored
   ns2 = if (score >61) then s2 else score -- check for going bust
 
   -------------------------------------------------------------------------------------------
 -- updateBoard 
 -- update the board after a play
 
 updateBoard :: Dom->End->Player->DomBoard->DomBoard
 
 updateBoard d e p b
  |e==L = playleft p d b
  |otherwise = playright p d b

  ------------------------------------------------------------------------------------------
 -- doms which will go left
 leftdrops :: Hand->DomBoard->Hand
 
 leftdrops h b = filter (\d -> goesLP d b) h
 
 -- doms which go right
 rightdrops :: Hand->DomBoard->Hand
 
 rightdrops h b = filter (\d -> goesRP d b) h 
 
 -------------------------------------------------
 -- 5s and 3s score for a number
  
 score53 :: Int->Int
 score53 n = 
  let 
   s3 = if (rem n 3)==0 then (quot n 3) else 0
   s5 = if (rem n 5)==0 then (quot n 5) else 0 
  in
   s3+s5
   
 ------------------------------------------------ 
 -- need comparing
 -- useful fn specifying what we want to compare by
 comparing :: Ord b=>(a->b)->a->a->Ordering
 comparing f l r = compare (f l) (f r)
 
 ------------------------------------------------
 -- scoreDom
 -- what will a given Dom score at a given end?
 -- assuming it goes
 
 scoreDom :: Dom->End->DomBoard->Int
 
 scoreDom d e b = scoreboard nb
                  where
                  (Just nb) = (playDom P1 d e b) -- player doesn't matter
 
 ----------------------------------------------------                 
 -- play to left - it will go
 playleft :: Player->Dom->DomBoard->DomBoard
 
 playleft p (d1,d2) InitBoard = Board (d1,d2) (d1,d2) [((d1,d2),p,1)]
 
 playleft p (d1,d2) (Board (l1,l2) r h)
  |d1==l1 = Board (d2,d1) r (((d2,d1),p,n+1):h)
  |otherwise =Board (d1,d2) r (((d1,d2),p,n+1):h)
  where
    n = maximum [m |(_,_,m)<-h] -- next drop number
    
 -- play to right
 playright :: Player->Dom->DomBoard->DomBoard
 
 playright p (d1,d2) InitBoard = Board (d1,d2) (d1,d2) [((d1,d2),p,1)]
 
 playright p (d1,d2)(Board l (r1,r2) h)
  |d1==r2 = Board l (d1,d2) (h++[((d1,d2),p,n+1)])
  |otherwise = Board l (d2,d1) (h++[((d2,d1),p,n+1)])
  where 
    n = maximum [m |(_,_,m)<-h] -- next drop number
 
 ------------------------------------------------------
 -- predicate - will given domino go at left?
 -- assumes a dom has been played
 
 goesLP :: Dom->DomBoard->Bool
 
 goesLP _ InitBoard = True
 
 goesLP (d1,d2) (Board (l,_) _ _) = (l==d1)||(l==d2)


 -- will dom go to the right?
 -- assumes a dom has been played
 
 goesRP :: Dom->DomBoard->Bool
 
 goesRP _ InitBoard = True
 
 goesRP (d1,d2) (Board _ (_,r) _) = (r==d1)||(r==d2)
 
 ------------------------------------------------

 -- playDom
 -- given player plays
 -- play a dom at left or right, if it will go

 
 playDom :: Player->Dom->End->DomBoard->Maybe DomBoard
 
 playDom p d L b
   |goesLP d b = Just (playleft p d b)
   |otherwise = Nothing
 
 playDom p d R b
   |goesRP d b = Just (playright p d b)
   |otherwise = Nothing
   
 ---------------------------------------------------    
 -- 5s & threes score for a board
 
 scoreboard :: DomBoard -> Int
 
 scoreboard InitBoard = 0

 scoreboard (Board (l1,l2) (r1,r2) hist)
  |length hist == 1 = score53 (l1+l2) -- 1 dom played, it's both left and right end
  |otherwise = score53 ((if l1==l2 then 2*l1 else l1)+ (if r1==r2 then 2*r2 else r2))   