% N-Queens problem
% Christophe Delord
% 24 May 2018

License
=======

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see http://www.gnu.org/licenses/.

Introduction
============

Here is a classic problem. The N-Queens problem consists in
placing N queens on a board without interfering. Two queens
must be on different rows, columns and diagonals.

The problem is to find all the possible configurations.

> import System.Environment
> import Data.List

Board representation
====================

There is only one queen on each row. So the board can be represented
by a list of integers, each integer is a row and its value is the position
of the queen in this row.

A board is $[\ldots q_i \ldots q_j \ldots]$.
By construction, $q_i$ and $q_j$ are not on the same row.
We must ensure that they are not on the same column or diagonal:

- $q_i$ and $q_j$ on the same column iif $q_i = q_j$
- $q_i$ and $q_j$ on the same diagonal iif $|q_i-q_j| = |i-j|$

Solver
======

`nqueens` takes an integer (N) and returns the list of all the solutions.
Let's notice that a solution is a permutation of $[1,N]$ because all columns
are different.
If we generate all the permutations we only have to check that two queens are
not on the same diagonal.

So, a solution is a permutation where $\forall (i,j) \in [1,N]^2, i \ne j \cdot |q_i-q_j| \ne |i-j|$

> nqueens :: Int -> [[Int]]
> nqueens n = [qs | qs <- perm [1..n], dontFight qs]
>     where
>         dontFight :: [Int] -> Bool
>         dontFight qs = and [ abs ((qs!!j) - (qs!!i)) /= j - i
>                            | i <- [0..n-2],
>                              j <- [i+1..n-1]
>                            ]
>         perm :: Eq a => [a] -> [[a]]
>         perm [] = [[]]
>         perm xs = [x:xs' | x <- xs, xs' <- perm (delete x xs)]

In fact this solution is not efficient.
It takes a factorial time because all permutations are generated.

A better solution is to check that a queen does not fight with others as soon as it is put
on the board instead of checking full boards only.

> nqueens' :: Int -> [[Int]]
> nqueens' n = solve n []
>     where

`solve` gets the number of queens to put on the board and the partially filled board:

- if all the queens have already been put, a solution has been found
- otherwise we try all the possible values:
    - a position is in $[1,n]$
    - and must not interfere with the current queens ($qs$)

>         solve :: Int -> [Int] -> [[Int]]
>         solve 0 qs = [qs]
>         solve i qs = concat [ solve (i-1) (q:qs)
>                             | q <- positions,
>                               dontFight q qs
>                             ]
>         positions = [1..n]

The new queen $q$ does not fight with any queens $q'$ if

- $\forall q' \in qs$
- let $i$ be the index of $q'$, $0$ being the index of $q$
- $q$ and $q'$ are not in the same column if $q'-q \ne 0$
- $q$ and $q'$ are not in the same diagonal if $q'-q \ne i \land q'-q \ne -i$

>         dontFight :: Int -> [Int] -> Bool
>         dontFight q qs = and [ (dq /= 0) && (dq /= i) && (dq /= -i)
>                              | (i,q') <- zip [1..] qs,
>                                let dq = q' - q
>                              ]

Main function
=============

The `main` function takes $N$ as an argument and prints all the solutions.

> main :: IO()
> main = do
>     [n] <- getArgs
>     let qss = nqueens' (read n :: Int)
>     putStrLn (n++"-queens problem\n")
>     mapM_ putBoard qss
>     putStrLn (show (length qss) ++ " solutions")
>
> putBoard = putStrLn . showBoard
>
> showBoard qs = dashes ++ concatMap showLine qs ++ dashes
>     where
>         dashes = "+" ++ replicate nqs '-' ++ "+\n"
>         showLine q = "|" ++ dots (q-1) ++ "Q" ++ dots (nqs-q) ++ "|\n"
>         dots k = replicate k '.'
>         nqs = length qs

Execution
=========

~~~~~
$ runhaskell nqueens.lhs 6

@(script.bash ".build/nqueens 6")
~~~~~

Source
======

The Haskell source code is here: [nqueens.lhs](nqueens.lhs)
