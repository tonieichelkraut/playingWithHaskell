module Bowling where

import Text.Read
import Control.Monad

newtype Roll = Roll{value::Int} deriving(Eq)

data Frame = Strike | Spare {firstRoll::Roll} | Normal {firstRoll::Roll,secondRoll::Roll} | Bonus{firstRoll::Roll,secondRoll::Roll,thridRoll::Roll}

instance Show Roll where
    show = show.value

int2Roll :: Int -> Roll
int2Roll i = Roll{value=i}

isValidInt :: Int -> Bool
isValidInt i = i>=0 && i<=10

string2Roll :: String -> Maybe Roll
string2Roll r = do
    i <- readMaybe r
    guard(isValidInt i)
    return (int2Roll i)


