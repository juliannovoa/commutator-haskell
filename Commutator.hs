module Commutator where

import Data.Char

type Operator = Char
type Alphabet = [Operator]
type OperatorSequence = [Operator]

match :: Operator -> Operator -> Bool
match x y = (toUpper x) == (toUpper y)

isInverse :: Operator -> Operator -> Bool
isInverse x y = (match x y) && ( ((isUpper x) && (isLower y))|| ((isLower x) && (isUpper y)) )

removeInverses :: OperatorSequence -> OperatorSequence
removeInverses [] = []
removeInverses [x] = [x]
removeInverses (x:y:ys)
    |isInverse x y = removeInverses ys
    |otherwise = checkBeginning x (removeInverses (y:ys))
    where checkBeginning x l
            |null l = x:[]
            |isInverse x (head l) = tail l
            |otherwise = x:l

removeOperator :: Operator -> OperatorSequence -> OperatorSequence
removeOperator x [y]
    |match x y = []
    |otherwise = [y]
removeOperator x (y:ys)
    |match x y = removeOperator x ys
    |otherwise = y : removeOperator x ys

testOperator :: Operator -> OperatorSequence -> Bool
testOperator x y = removeInverses (removeOperator x y) == []

isCommutator :: Alphabet -> OperatorSequence -> Bool
isCommutator alphabet seq = (seq == removeInverses seq) && test alphabet seq
    where test (x:xs) seq 
            |null xs = testOperator x seq
            |otherwise = testOperator x seq && test xs seq