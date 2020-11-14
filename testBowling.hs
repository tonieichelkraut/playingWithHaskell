import Test.HUnit
import Bowling

testDataString2Roll :: [([Char], Maybe Roll)]
testDataString2Roll = [
    ("0",Just Roll{value=0}),
    ("5",Just Roll{value=5}),
    ("10",Just Roll{value=10}),
    ("11",Nothing),
    ("-1",Nothing),
    ("x",Nothing)
    ]
                    
message :: String -> Maybe Roll -> String
message s r = "computing " ++ show r ++ " from " ++ s ++ " failed."

test_string2Roll :: String -> Maybe Roll -> Test
test_string2Roll s r = TestCase (assertEqual (message s r) r (string2Roll s))

tests_string2Roll :: Test
tests_string2Roll = TestList (map (uncurry test_string2Roll) testDataString2Roll)


main :: IO Counts
main = runTestTT tests_string2Roll
