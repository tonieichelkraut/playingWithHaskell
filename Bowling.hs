module Bowling where

import Text.Read
import Control.Monad
import Data.List

newtype Roll = Roll{value::Int} deriving(Eq)

data CompleteFrame = Strike | Spare {firstRoll::Roll} | Normal {firstRoll::Roll,secondRoll::Roll} | Bonus{firstRoll::Roll,secondRoll::Roll,thridRoll::Roll} deriving(Eq)
type CurrentFrame = [Roll]

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