% Write a module boolean.erl that takes logical expressions and Boolean values
% (represented as the atoms true and false) and returns their Boolean result.
% The functions you write should include b_not/1, b_and/2, b_or/2, and b_nand/2.
% You should not use the logical constructs and, or, and not, but instead use
% pattern matching to achieve your goal.

% Hint: implement b_nand/2 using b_not/1 and b_and/2.

-module(boolean).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(true)  -> false;
b_not(false) -> true.

b_and(false, false) -> false;
b_and(false, true) -> false;
b_and(true, false) -> false;
b_and(true, true) -> true.

b_or(A, B) -> b_not(b_and(b_not(A), b_not(B))).

b_nand(A, B) -> b_not(b_and(A, B)).
