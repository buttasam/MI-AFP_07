module CalculatorSpec (spec) where

import Test.Hspec

import Calculator

spec :: Spec
spec = describe "calculate" $ do
        context "without any bad input" $ do
          it "does basic math" $ do
            calculate "ADD 5 3" `shouldBe` Right 8
            calculate "ADD 1.5 3.4" `shouldBe` Right 4.9
            calculate "ADD (-2) 1" `shouldBe` Right (-1)
            calculate "SUB 5 3" `shouldBe` Right 2
            calculate "SUB (-2) 1" `shouldBe` Right (-3)
            calculate "MUL 5 3" `shouldBe` Right 15
            calculate "MUL 0.5 4" `shouldBe` Right 2
            calculate "MUL (-2) 7" `shouldBe` Right (-14)
            calculate "DIV 15 3" `shouldBe` Right 5
            calculate "DIV (-2) 4" `shouldBe` Right (-0.5)
          it "does other functions with two arguments" $ do
            calculate "GCD 5 3" `shouldBe` Right 1
            calculate "GCD 15 10" `shouldBe` Right 5
            calculate "GCD 234 192" `shouldBe` Right 6
            calculate "GCD 5 0" `shouldBe` Right 5
            calculate "POW 0 5" `shouldBe` Right 0
            calculate "POW (-3) 0" `shouldBe` Right 1
            calculate "POW 2 5" `shouldBe` Right 32
            calculate "LOG 2 32" `shouldBe` Right 5
            calculate "LOG 5 1" `shouldBe` Right 0
          it "does other functions with one argument" $ do
            calculate "SQRT 25" `shouldBe` Right 5
            calculate "SQRT 0" `shouldBe` Right 0
            calculate "SQRT 145" `shouldBe` Right (sqrt 145)
            calculate "LOG 100" `shouldBe` Right 2
            calculate "LOG 1" `shouldBe` Right 0
            calculate "SIN 0" `shouldBe` Right 0
            calculate "SIN 10" `shouldBe` Right (sin 10)
            calculate "SIN -10" `shouldBe` Right (sin (-10))
            calculate "COS 0" `shouldBe` Right 1
            calculate "COS 10" `shouldBe` Right (cos 10)
            calculate "COS -10" `shouldBe` Right (cos (-10))
          it "knows constant PI and -PI" $ do
            calculate "PI" `shouldBe` Right pi
            calculate "-PI" `shouldBe` Right (-pi)
          it "can use constants as arguments" $ do
            calculate "ADD PI -PI" `shouldBe` Right 0
            calculate "DIV -PI PI" `shouldBe` Right (-1)
            calculate "SIN PI" `shouldBe` Right 0
            calculate "COS -PI" `shouldBe` Right (-1)
        context "with some typos and basic erros" $ do
          it "detects no input" $
            calculate "" `shouldBe` Left "No input"
          it "detects unknown instructions" $ do
            calculate "PIE 5" `shouldBe` Left "Unknown operation: PIE"
            calculate "KOS PI" `shouldBe` Left "Unknown operation: KOS"
            calculate "LG 0" `shouldBe` Left "Unknown operation: KOS"
          it "detects unknown constants" $ do
            calculate "PIE" `shouldBe` Left "Unknown constant: PIE"
            calculate "SIN PIE" `shouldBe` Left "Unknown constant: PIE"
        context "with non-numeric and missing arguments" $ do
          it "detects not-a-number inputs (and not a constant)" $ do
            calculate "ADD 5b 1" `shouldBe` Left "Not a number: 5b"
            calculate "ADD -p b" `shouldBe` Left "Not a number: -p"
            calculate "MUL PI1" `shouldBe` Left "Not a number: PI1"
          it "detects unsufficient number of arguments" $ do
            calculate "MUL 5" `shouldBe` Left "Missing operand/s (1 given, 2 required)"
            calculate "GCD 5" `shouldBe` Left "Missing operand/s (1 given, 2 required)"
            calculate "SIN" `shouldBe` Left "Missing operand/s (0 given, 1 required)"
            calculate "LOG" `shouldBe` Left "Missing operand/s (0 given, 1 required)"
        context "mathematically incorrect inputs" $
          it "detects all numerical errors" $ do
            calculate "DIV 5 0" `shouldBe` Left "DIV - Y cannot be 0"
            calculate "DIV 0 0" `shouldBe` Left "DIV - Y cannot be 0"
            calculate "GCD 5 20" `shouldBe` Left  "GCD - X must be greater or equal to Y"
            calculate "GCD 2.5 3.0" `shouldBe` Left  "GCD - X and Y must be natural numbers"
            calculate "POW 0 0" `shouldBe` Left  "POW - X and Y cannot be both 0"
            calculate "LOG 0 0" `shouldBe` Left  "LOG - X and B must be greater than 0"
            calculate "LOG 0 10" `shouldBe` Left  "LOG - B must be greater than 0"
            calculate "LOG 7 0" `shouldBe` Left  "LOG - X must be greater than 0"
            calculate "LOG 0" `shouldBe` Left  "LOG - X must be greater than 0"
            calculate "SQRT -1" `shouldBe` Left  "SQRT - X must be greater or equal to 0"
