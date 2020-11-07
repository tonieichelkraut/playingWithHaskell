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
parseString = sequence . map parseChar

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
   | l <= n = (s+(digit2Number n),n)
   | otherwise = (s-(digit2Number n),n)

main :: IO ()
main = do
    let string = "VXXVIII"
        roman = parseString string
        number = fmap (foldr combinator (0,I)) roman
    print roman
    print number
    return ()
