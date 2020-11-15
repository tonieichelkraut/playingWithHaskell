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

testsString2Roll :: Test
testsString2Roll = TestList (map (uncurry testString2Roll) testDataString2Roll)

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

testsAddRoll :: Test
testsAddRoll = TestList (map (uncurryN testAddRoll) testDataAddRoll)

runTests :: IO Counts
runTests = runTestTT.TestList $ [testsString2Roll,testsAddRoll]

play :: Game -> IO Game
play g0 = do
    putStrLn "Enter Roll: "
    r <- readRoll
    let g1 = maybe g0 (addRoll g0) r
    print $ showGame g1
    play g1