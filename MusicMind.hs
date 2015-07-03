--  MusicMind.hs
--  A MusicMind guessing game program.
--  Author: QINGYANG HONG 
--  ID: 629379

--  The program is a solution to the MusicMind guessing game.
--  The basic idea is to build a list of all possible guesses,
--  which is stored in the GameState and delete the impossible
--  guesses every time that the nextGuess receivces an answer.
--  The next guess will be in the scope of remained possible 
--  guesses. Finally,the last possible guess is the answer. 

--  p.s:The program can achieve 4.509774436090225 average 
--  guesses by testing against all 1330 cases.

module MusicMind (initialGuess, nextGuess, GameState) where
import Data.List

--  the GameState, which stores the possible guesses. 
type GameState = [[String]]

--  the iniGamestate, which stores all 1330 possible guesses at
--  the beginning.
iniGamestate =
    guessfilter baseGuess
       where pitches = ["A1","A2","A3","B1","B2","B3","C1",
                        "C2","C3","D1","D2","D3","E1","E2",
                        "E3","F1","F2","F3","G1","G2","G3"]
             baseGuess = [[x,y,z]|x<-pitches,y<-pitches,
                                                z<-pitches]

--  orders and filters out the repetitive pitches.
guessfilter :: [[String]] -> [[String]]
guessfilter [] = [] 
guessfilter (x : xs)  
    | (x !! 0)>(x !! 1) && (x !! 1)>( x !! 2) = [ x ] ++ (guessfilter xs)
    | otherwise = guessfilter xs

--  takes no input arguments and returns an initial guess and
--  a GameState. The initial guess is ["D2", "E3", "G2"], which
--  can give a relatively better division for the future guesses.
initialGuess :: ([String], GameState)
initialGuess = (["D2", "E3", "G2"], iniGamestate)

--  takes as input the previous guess and the game state and the answer 
--  from the previous guess showing the correct pitches, notes, and octaves 
--  and returns the next guess and the next game state. Importantly, 
--  impossible guesses will be removed from GameState.
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (guess, other) (a, b, c) = (guess', other')                                
                     where other' = octavesFilter c a nFiltered guess
                           nFiltered = notesFilter b a pFiltered guess
                           pFiltered = pitchFilter a other guess
                           guess' = head other'

--  filters out the impossible guesses in the game state according 
--  to the number of correct pitches in the answer by keeping the 
--  guesses, which have the same number of correct pitches.
pitchFilter :: Int -> [[String]] -> [String] -> [[String]]
pitchFilter _ [] [] = []
pitchFilter _ [] _ = []
pitchFilter crctPitch (x : xs) guess 
    | (numOfMatch guess x) == crctPitch = 
        [x] ++ pitchFilter crctPitch xs guess
    | otherwise = pitchFilter crctPitch xs guess

--  filters out the impossible guesses in the game state according 
--  to the sum of correct notes and pitches in the answer by 
--  keeping the guesses, which have the same sum of correct notes 
--  and pitches.
notesFilter :: Int -> Int -> [[String]] -> [String] -> [[String]]
notesFilter crctnotes crctPitch [] _ = []
notesFilter crctnotes crctPitch (x : xs) guess  
    | (numOfMatch guessnotes xnotes) == sum = 
        [x] ++ notesFilter crctnotes crctPitch xs guess
    | otherwise = notesFilter crctnotes crctPitch xs guess
        where sum = crctnotes+crctPitch
              guessnotes = [[(guess!!0)!!0]]
                            ++[[(guess!!1)!!0]]
                            ++[[(guess!!2)!!0]]
              xnotes = [[(x!!0)!!0]]++[[(x!!1)!!0]]++[[(x!!2)!!0]]

--  filters out the impossible guesses in the game state according 
--  to the sum of correct octaves and pitches in the answer by 
--  keeping the guesses, which have the same sum of correct octaves 
--  and pitches.
octavesFilter :: Int -> Int -> [[String]] -> [String] -> [[String]]
octavesFilter crctoctaves crctPitch [] _ = []
octavesFilter crctoctaves crctPitch (x : xs) guess 
    | (numOfMatch guessoctaves xoctaves) == sum = 
        [x] ++ octavesFilter crctoctaves crctPitch xs guess
    | otherwise = octavesFilter crctoctaves crctPitch xs guess
        where sum = crctoctaves + crctPitch
              guessoctaves = [[(guess!!0)!!1]]
                              ++[[(guess!!1)!!1]]
                              ++[[(guess!!2)!!1]]
              xoctaves = [[(x!!0)!!1]]++[[(x!!1)!!1]]++[[(x!!2)!!1]]

--  calculates the number of successful matchings between a baseguess and 
--  the input guess.
numOfMatch :: [String] -> [String] -> Int
numOfMatch [] [] = 0
numOfMatch _ [] = 0
numOfMatch [] _ = 0
numOfMatch (x:xs) pat
    | isInfixOf [x] pat = numOfMatch xs (deleteMatch x pat) + 1
    | otherwise = numOfMatch xs pat

--  deletes a pitch, a note or an octave from a list, if it is matched. 
deleteMatch _ [] = []   
deleteMatch x (y:ys)        
    |  x == y = ys       
    | otherwise = [y] ++ (deleteMatch x ys)

