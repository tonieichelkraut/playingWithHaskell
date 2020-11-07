import Control.Monad
import Data.List.Safe as SafeList

data Digit = I | V | X | L | C | D | M deriving(Eq,Ord,Show)

parseChar :: Char -> Maybe Digit
parseChar c
    | c=='I' = return I
    | c=='V' = return V
    | c=='X' = return X
    | c=='L' = return L
    | c=='C' = return C
    | c=='D' = return D
    | c=='M' = return M
    | otherwise = Nothing

parseString :: String -> Maybe [Digit]
parseString = mapM parseChar

zeroThroughNine :: Digit -> Digit -> Digit -> [[Digit]]
zeroThroughNine o f t = [[],[o],[o,o],[o,o,o],[o,f],[f],[f,o],[f,o,o],[f,o,o,o],[o,t]]

validate :: [Digit] -> [[Digit]]
validate ds = do
  one <- zeroThroughNine I V X
  ten <- zeroThroughNine X L C
  hundred <- zeroThroughNine C D M
  thousand <- [[],[M],[M,M],[M,M,M]]
  let r = thousand ++ hundred ++ ten ++ one
  guard (r == ds)
  return r

stringToRoman :: String -> Maybe [Digit]
stringToRoman s = parseString s >>= SafeList.head . validate

romanToInt :: [Digit] -> Int
romanToInt r = let (n,_) = foldr combinator (0,I) r
               in n

digit2Number :: Digit -> Int
digit2Number I =    1
digit2Number V =    5
digit2Number X =   10
digit2Number L =   50
digit2Number C =  100
digit2Number D =  500
digit2Number M = 1000

combinator :: Digit -> (Int,Digit) -> (Int,Digit)
combinator n (s,l)
   | l <= n = (s+digit2Number n,n)
   | otherwise = (s-digit2Number n,n)

main :: IO ()
main = do
    putStrLn "Enter roman number:"
    string <- getLine
    let validatedRoman = stringToRoman string
        number = romanToInt <$> validatedRoman
    print validatedRoman
    print number
