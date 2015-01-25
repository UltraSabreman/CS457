module Test where

fizzbuzz =  [ if x `mod` 3 == 0 then (if x `mod` 5 == 0 then "FB" else "F") else if x `mod` 5 == 0 then "B" else show x | x <- [0..100]]
