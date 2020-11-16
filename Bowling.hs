module Bowling where

import Text.Read
import Control.Monad
import Data.List

newtype Roll = Roll{value::Int} deriving(Eq)

data CompleteFrame = Strike | Spare {firstRoll::Roll} | Normal {firstRoll::Roll,secondRoll::Roll} | Bonus{firstRoll::Roll,secondRoll::Roll,thridRoll::Roll} deriving(Eq)
type CurrentFrame = [Roll]
type FrameScores = [Int]

type Game = ([CompleteFrame],CurrentFrame)

instance Show Roll where
    show = show.value

instance Show CompleteFrame where
    show f = case f of
              Strike ->             " X "
              Spare{firstRoll=f} -> show f++" /"
              Normal{firstRoll=f,secondRoll=s} -> show f++" "++show s

showGame :: Game -> String
showGame (fs,[]) = foldl (\a b -> a++"|"++b) "" (map show fs)
showGame (fs,[c]) = foldl (\a b -> a++"|"++b) "" (map show fs) ++ "|" ++show c

readRoll :: IO (Maybe Roll)
readRoll = string2Roll <$> getLine

roll :: Int -> Roll
roll i = Roll{value=i}

normal :: Int -> Int -> CompleteFrame
normal f s = Normal{firstRoll=roll f,secondRoll=roll s}

spare :: Int -> CompleteFrame
spare f = Spare{firstRoll=roll f}

isValidInt :: Int -> Bool
isValidInt i = i>=0 && i<=10

string2Roll :: String -> Maybe Roll
string2Roll r = do
    i <- readMaybe r
    guard(isValidInt i)
    return (roll i)

addRoll :: Game -> Roll -> Game
addRoll g0@(fs,[]) r =  let score = value r
                            update
                               | score < 10  = (fs,[r])
                               | score == 10 = (fs ++ [Strike],[])
                               | otherwise   = g0
                        in update
addRoll g0@(fs,[f]) s = let combined = value f + value s
                            includeRollAsNormal =(fs ++ [Normal{firstRoll=f,secondRoll=s}],[])
                            includeRollAsSpare =(fs ++ [Spare{firstRoll=f}],[])
                            ignoreRoll = g0
                            update
                               | combined < 10  = includeRollAsNormal
                               | combined == 10 = includeRollAsSpare
                               | otherwise      = ignoreRoll
                        in update

scoreFrame :: CompleteFrame -> Int
scoreFrame Strike = 10
scoreFrame Spare{firstRoll=_} = 10
scoreFrame Normal{firstRoll=f,secondRoll=s} = value f + value s

scoreGame :: Game -> FrameScores
scoreGame ([],c) = map value c
scoreGame (fs,c) = let scores = tail $ scanl (\s f-> scoreFrame f+s) 0 fs
                       last_s = last scores
                       c_score = map (\r -> last_s + value r) c
                   in scores ++ c_score

showScore :: Int -> String
showScore i = let s = show i
                  l = length s
              in case l of
                      1 -> "  "++s
                      2 -> " "++s
                      _ -> s

showGameScore :: FrameScores -> String
showGameScore [] = ""
showGameScore fs = foldl (\b a-> b ++ "|" ++ showScore a) "" fs 