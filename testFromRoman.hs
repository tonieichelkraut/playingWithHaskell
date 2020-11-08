import Test.HUnit
import FromRoman

validInputs = [("I",Just 1),
               ("II",Just 2),
               ("III",Just 3),
               ("IV",Just 4),
               ("",Just 0),
               ("A",Nothing)]

message s n = "computing " ++ show n ++ " from roman " ++ s ++ " failed."

romans s n = TestCase (assertEqual (message s n) (romanString2Int s) n)

tests = TestList (map (uncurry romans) validInputs)

main :: IO Counts
main = runTestTT tests
