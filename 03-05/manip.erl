% Write a function that, given a list of integers and an integer,
% will return all integers smaller than or equal to that integer.

% Write a function that, given a list, will reverse the order of the elements.

% Write a function that, given a list of lists, will concatenate them.
% Hint: you will have to use a help function and concatenate the lists in several steps.

% Write a function that, given a list of nested lists, will return a flat list.
% Hint: use concatenate to solve flatten.

-module(manip).
-export([ reverse/1, filter/2, concatenate/1, flatten/1 ]).
-include_lib("eunit/include/eunit.hrl").

filter([], _N) ->
  [];
filter([ H | T ], N) when H =< N ->
  [ H | filter(T, N) ];
filter([ _H | T ], N) ->
  filter(T, N).

reverse([], Rev) ->
  Rev;
reverse([ H | T ], Rev) ->
  reverse(T, [ H | Rev ]).
reverse(L) ->
  reverse(L, []).

concatenate([], L) ->
  L;
concatenate([ H | T ], L) ->
  [ H | concatenate(T, L) ].

concatenate([]) ->
  [];
concatenate([ H | T ]) ->
  concatenate(H, concatenate(T)).

flatten([]) -> [];
flatten([ H | T ]) when is_list(H) ->
  concatenate(flatten(H), flatten(T));
flatten([ H | T ]) ->
  concatenate([ H ], flatten(T)).

filter_test() ->
  [] = filter([], 1),
  [] = filter([ 1, 2, 3 ], -1),
  [ 1, 2, 3 ] = filter([ 1, 2, 3 ], 3).

reverse_test() ->
  [] = reverse([]),
  [ 3, 2, 1 ] = reverse([ 1, 2, 3 ]).

concatenate_test() ->
  [] = concatenate([]),
  [ 1, 2, 3 ] = concatenate([ [ 1 ], [ 2 ], [ 3 ] ]),
  [ 1, [ 2 ], 3 ] = concatenate([ [ 1 ], [ [ 2 ] ], [ 3 ] ]).

flatten_test() ->
  [] = flatten([]),
  [ 1, 2, 3, 4, 5, 6, 7, 8 ] = flatten([ [ 1 ], [ [ 2, 3 ], [ 4, 5 ] ], [ [ [ 6 ], [ 7 ], [ 8 ] ] ] ]).

