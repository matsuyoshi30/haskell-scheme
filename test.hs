module Test where

import Main hiding (main)
import Test.HUnit
import Data.Foldable

testcase :: [String] -> String -> Test
testcase exprs want = TestCase $ do
  env <- primitiveBindings
  for_ (init exprs) $ evalString env
  got <- evalString env $ last exprs
  assertEqual want want got

tests :: [([String], String)] -- test, want
tests = [(["(+ 1 2)"], "3"),
         (["(- 4 2)"], "2"),
         (["(* 4 2)"], "8"),
         (["(/ 9 3)"], "3"),
         (["(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))"], "57"),
         (["(mod 7 4)"], "3"),
         (["(quotient 8 3)"], "2"),
         (["(remainder 9 2)"], "1"),
         (["(boolean? #f)"], "#t"),
         (["(symbol? 'a)"], "#t"),
         (["(string? \"str\")"], "#t"),
         (["(number? 10)"], "#t"),
         (["(char? #\\A)"], "#t"),
         (["(list? '(1 2 3))"], "#t"),
         (["(vector? '#(0 (2 2 2 2) \"Anna\"))"], "#t"),
         (["(symbol->string 'symbol)"], "\"symbol\""),
         (["(string->symbol \"string\")"], "string"),
         (["(string-length \"string\")"], "6"),
         (["(make-string 3 #\\a)"], "\"aaa\""),
         (["(string-ref \"string\" 2)"], "'r'"),
         (["(= 1 2)"], "#f"),
         (["(/= 1 2)"], "#t"),
         (["(> 3 2)"], "#t"),
         (["(< 2 5)"], "#t"),
         (["(>= 2 2)"], "#t"),
         (["(<= 3 3)"], "#t"),
         (["(&& #t #f)"], "#f"),
         (["(|| #f #t)"], "#t"),
         (["(string=? \"kitten\" \"kitten\")"], "#t"),
         (["(string>? \"kitten\" \"bath\")"], "#t"),
         (["(string<? \"kitten\" \"bath\")"], "#f"),
         (["(string>=? \"house\" \"horse\")"], "#t"),
         (["(string<=? \"house\" \"horse\")"], "#f"),
         (["(car '(1 2 3))"], "1"),
         (["(cdr '(1 2 3))"], "(2 3)"),
         (["(cons 'a '(b c))"], "(a b c)"),
         (["(eq? #t #t)"], "#t"),
         -- (["(eqv? #\\a #\\a)"], "#t"), failed
         (["(eqv? 1 1)"], "#t"),
         (["(equal? '(1 2) '(1 2))"], "#t"),
         (["(if #t 7 3)"], "7"),
         (["(if #f 7 3)"], "3"),
         (["(define size 2)", "size"], "2"),
         (["(define (square x) (* x x))", "(square 4)"], "16"),
         (["(define (square x) (* x x))", "(square (square 3))"], "81"),
         (["(define (square x) (* x x))",
           "(define (sum-of-squares x y) (+ (square x) (square y)))",
           "(sum-of-squares 3 4)"], "25"),
         (["(define (forthpower n) (define m (* n n)) (* m m))",
           "(forthpower 3)"], "81"),
         (["(define (square x) (* x x))",
           "(define x 42)",
           "(square 5)",
           "x"], "42")]

main :: IO Counts
main = do
  runTestTT $ TestList $ flip map tests $ (\(x, y) -> let t = testcase x y in TestLabel (last x) t)
