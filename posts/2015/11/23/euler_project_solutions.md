---
title: 오일러 프로젝트 haskell solutions
date: 2015-11-23T01:05:06+09:00
published: true
tags: haskell, euler, solutions
---

### Problem 46

It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

### Solution

let square x y = map (\z -> x + 2^z) [1..y]

map (\x -> square x) $ filter isPrime [1..100]



### Problem 47

The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?

### Solution

import Data.List.Split -- chunksOf
import  Data.List.Grouping -- splitEvery

let divisors x = 1:[ y | y <- [2..(x `div` 2)], x `mod` y == 0] ++ [x]

let isPrime x = divisors x == [1,x]

filter (\ x -> (sum(x) - head(x) * 4) == 4) $ chunksOf 4 $ take 10000 [n | n <- [1..], (length $ filter isPrime $ divisors n) == 5]
