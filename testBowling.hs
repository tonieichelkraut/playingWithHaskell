{-# LANGUAGE FlexibleContexts #-}
import Test.HUnit
import Bowling
import Data.Tuple.Curry
import Data.Maybe

testDataString2Roll :: [([Char], Maybe Roll)]
testDataString2Roll = [
    ("0",Just (roll 0)),
    ("5",Just (roll 5)),
    ("10",Just (roll 10)),
    ("11",Nothing),
    ("-1",Nothing),
    ("x",Nothing)
    ]

message :: String -> Maybe Roll -> String
message s r = "computing " ++ show r ++ " from " ++ s ++ " failed."

testString2Roll :: String -> Maybe Roll -> Test
testString2Roll s r = TestCase (assertEqual (message s r) r (string2Roll s))

testDataAddRoll :: [(Roll, Game, Game)]
testDataAddRoll = [(roll 0,([],[]),([],[roll 0])),
                   (roll 3,([],[roll 0]),([normal 0 3],[])),
                   (roll 6,([normal 0 3],[roll 4]),([normal 0 3, spare 4],[])),
                   (roll 9,([normal 0 3],[roll 1]),([normal 0 3, spare 1],[])),
                   (roll 10,([normal 0 3],[roll 0]),([normal 0 3, spare 0],[])),
                   (roll 10,([normal 0 3],[]),([normal 0 3, Strike],[])),
                   (roll 11,([normal 0 3],[]),([normal 0 3],[])),
                   (roll 6,([],[roll 5]),([],[roll 5]))]

testAddRoll :: Roll -> Game -> Game -> Test
testAddRoll r g0 g1 = TestCase(assertEqual "Did not correctly add roll to game." g1 (addRoll g0 r))

tests :: Curry (a -> Test) b => (b, [a]) -> Test
tests (f,d) = TestList (map (uncurryN f) d)

testsString2Roll :: Test
testsString2Roll = tests (testString2Roll,testDataString2Roll)--TestList (map (uncurryN testString2Roll) testDataString2Roll)

testsAddRoll :: Test
testsAddRoll = tests (testAddRoll,testDataAddRoll)--TestList (map (uncurryN testAddRoll) testDataAddRoll)

testDataScoreGame = [(([],[roll 3]),[3]),
                     (([],[]),[]),
                     (([normal 3 4,normal 2 1],[]),[7,10]),
                     (([normal 3 4,normal 2 1],[roll 3]),[7,10,13]),
                     (([normal 3 4,Strike,normal 4 5],[]),[7,26,35])]

testScoreGame :: Game -> FrameScores -> Test
testScoreGame g s = TestCase(assertEqual "Did not correctly score game." s (scoreGame g))

testsScoreGame :: Test
testsScoreGame = tests (testScoreGame,testDataScoreGame)

runTests :: IO Counts
runTests = runTestTT.TestList $ [testsString2Roll,testsAddRoll,testsScoreGame]

play :: Game -> IO Game
play g0 = do
    putStrLn "Enter Roll: "
    r <- readRoll
    let g1 = maybe g0 (addRoll g0) r
    let s = scoreGame g1
    print "|1. |2. |3. |4. |5. |6. |7. |8. |9. |10."
    print $ showGame g1
    print $ showGameScore s
    play g1