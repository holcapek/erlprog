% Write a function that prints out the integers between 1 and N.
% Hint: use io:format("Number:~p~n",[N]).

% Write a function that prints out the even integers between 1 and N.
% Hint: use guards.

-module(sideff).
-export([printall/1, printeven/1]).

printall(N, N) ->
  io:format("Number:~p~n", [ N ]);
printall(N, I) when I < N ->
  io:format("Number:~p~n", [ I ]),
  printall(N, I + 1).
printall(N) when N > 0 ->
  printall(N, 1).

printeven(N, I) when I > N ->
  ok;
printeven(N, I) when I rem 2 == 0 ->
  io:format("Number:~p~n", [ I ]),
  printeven(N, I + 2);
printeven(N, I) when I rem 2 == 1 ->
  printeven(N, I + 1).
printeven(N) when N > 0 ->
  printeven(N, 2).
