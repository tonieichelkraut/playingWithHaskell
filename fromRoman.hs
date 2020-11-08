import Control.Monad
import Data.List.Safe as SafeList
import Data.List
import Data.Maybe

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
--zeroThroughNine o f t = [[],[o],[o,o],[o,o,o],[o,f],[f],[f,o],[f,o,o],[f,o,o,o],[o,t]]
zeroThroughNine o f t = [[o,t],[o,f],[f,o,o,o],[f,o,o],[f,o],[f],[o,o,o],[o,o],[o],[]]

thousands :: [[Digit]]
thousands = [[M,M,M],[M,M],[M],[]]

validate :: [Digit] -> [[Digit]]
validate ds = do
  one <- zeroThroughNine I V X
  ten <- zeroThroughNine X L C
  hundred <- zeroThroughNine C D M
  thousand <- thousands
  let r = thousand ++ hundred ++ ten ++ one
  guard (r == ds)
  return r

eitherStrip :: [Digit] -> [Digit] -> Either [Digit] [Digit]
eitherStrip ds p = case stripPrefix p ds of
                      Just s -> Left s
                      Nothing -> Right ds

eitherStripFromList :: [[Digit]] -> [Digit] -> Maybe [Digit]
eitherStripFromList ps ds = case foldM eitherStrip ds ps of
                              Left s -> Just s
                              _ -> Nothing

validate' :: [Digit] -> Maybe [Digit]
validate' ds = let ones = zeroThroughNine I V X
                   tens = zeroThroughNine X L C
                   hundreds = zeroThroughNine C D M
                in eitherStripFromList thousands ds >>=
                   eitherStripFromList hundreds >>=
                   eitherStripFromList tens >>=
                   eitherStripFromList ones >>=
                   guard.null >>=
                   const (return ds)

stringToRoman :: String -> Maybe [Digit]
stringToRoman s = parseString s >>= SafeList.head . validate

stringToRoman' :: String -> Maybe [Digit]
stringToRoman' s = parseString s >>= validate'

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
        validatedRoman' = stringToRoman' string
        number = romanToInt <$> validatedRoman
    putStrLn "First validation result:"
    print validatedRoman
    putStrLn "Second validation result:"
    print validatedRoman'
    putStrLn "This roman number equals:"
    print number
