module Calculator (calculator, calculate) where

import Data.List.Split

type Calculation = Either String Double


data Operation = UnaryOp { unName :: String, arg :: Double }
               | BinaryOp { binName :: String, arg1 :: Double, arg2 :: Double}
               | Constant { unName :: String }
               deriving Show

-- Use monads, and functors and/or applicative to make the code simple!

-- Devise own operations with errors, here are msgs for you
e0 x = "Unknown operation: " ++ show x
e1 x = "Unknown constant: " ++ show x
e2 x = "Not a number:" ++ show x
e3 g r = "Missing operand/s (" ++ show g ++ " given, " ++ show r ++ " required)"
e4   = "DIV - Y cannot be 0"
e5   = "GCD - X must be greater or equal to Y"
e6   = "GCD - X and Y must be natural numbers"
e7   = "POW - X and Y cannot be both 0"
e8   = "LOG - X and B must be greater than 0"
e9   = "LOG - B must be greater than 0"
e10  = "LOG - X must be greater than 0"
e11  = "SQRT - X must be greater or equal to 0"
e12  = "No input"

-- | Function do to the calculation
calculate :: String -> Calculation
calculate input
      | input == "" = Left e12
      | otherwise = Right (evalOperation (stringToOperation input))

stringToOperation :: String -> Operation
stringToOperation str
    | size == 1 = Constant (head arr)
    | size == 2 = UnaryOp (head arr) (evalConstantOperation (Constant (arr !! 1)))
    | size == 3 = BinaryOp (head arr) (evalConstantOperation (Constant (arr !! 1))) (evalConstantOperation (Constant (arr !! 2)))
    | otherwise = error "invalid input"
      where
        arr = splitOn " " str
        size = length arr

evalBinOperation :: Operation -> Double
evalBinOperation (BinaryOp binName arg1 arg2) = case binName of
                 "ADD" -> arg1 + arg2
                 "SUB" -> arg1 - arg2
                 "MUL" -> arg1 * arg2
                 "DIV" -> arg1 / arg2
                 "GCD" -> fromIntegral (gcd floorArg1 floorArg2)
                 "POW" -> fromIntegral (floorArg1 ^ floorArg2)
                 "LOG" -> logBase arg1 arg2
                 where
                   floorArg1 = floor arg1
                   floorArg2 = floor arg2


evalUnaryOperation :: Operation -> Double
evalUnaryOperation (UnaryOp unName arg) = case unName of
                  "SQRT" -> sqrt arg
                  "LOG" -> logBase 10 arg
                  "SIN" -> persitionZero (sin arg)
                  "COS" -> cos arg
                  where
                    persitionZero v = if abs(v) < 0.00001 then 0 else v


evalConstantOperation :: Operation -> Double
evalConstantOperation (Constant c) = case c of
                  "PI" -> pi
                  "-PI" -> -pi
                  _ -> read c


evalOperation :: Operation -> Double
evalOperation b@(BinaryOp _  _  _ ) = evalBinOperation b
evalOperation u@(UnaryOp _  _ ) = evalUnaryOperation u
evalOperation c@(Constant _  ) = evalConstantOperation c

-- | Help for you and the users
help :: IO ()
help = do
  putStrLn "-------------------------"
  putStrLn "Binary operations:"
  putStrLn " ADD X Y"
  putStrLn " SUB X Y"
  putStrLn " MUL X Y"
  putStrLn " DIV X Y" -- Y /= 0
  putStrLn " GCD X Y" -- X > Y, positive
  putStrLn " POW X Y" -- X /= 0 || Y /= 0
  putStrLn " LOG B X" -- X > 0 && B > 0
  putStrLn "Unary functions:"
  putStrLn " SQRT X"  -- X >= 0
  putStrLn " LOG X"   -- B = 10, X > 0
  putStrLn " SIN X"
  putStrLn " COS X"
  putStrLn "Constants:" -- constant are just [A-Z]+ strings
  putStrLn " PI (-PI)"
  putStrLn "Others:"
  putStrLn " ?"
  putStrLn "-------------------------"

-- | CLI for calculator
calculator :: IO ()
calculator = do
    putStrLn "Even more Basic Calculator (use '?' for help)"
    -- TODO: implement it with some loop?!
    help
    putStrLn "Bye!"
