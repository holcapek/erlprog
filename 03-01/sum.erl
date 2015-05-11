% Write a function sum/1 which, given a positive integer N,
% will return the sum of all the integers between 1 and N.

% Write a function sum/2 which, given two integers N and M,
% where N =< M, will return the sum of the interval between N and M.
% If N > M, you want your process to terminate abnormally.

-module(sum).
-export([sum/1, sum_tail/1, sum2/2, sum2_tail/2]).

sum(1) -> 1;
sum(N) when N > 1 -> N + sum(N - 1).

sum_tail(1, A) -> 1 + A;
sum_tail(N, A) -> sum_tail(N - 1, N + A).

sum_tail(N) when N > 0 -> sum_tail(N, 0).

sum2(N, N) when N > 0 -> N;
sum2(N, M) when N > 0, N < M -> N + sum2(N + 1, M).

sum2_tail(N, N, A) -> N + A;
sum2_tail(N, M, A) -> sum2_tail(N, M - 1, A + M).

sum2_tail(N, M) when N > 0, N =< M -> sum2_tail(N, M, 0).
