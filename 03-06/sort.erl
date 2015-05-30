% Quicksort
% The head of the list is taken as the pivot; the list is then split
% according to those elements smaller than the pivot and the rest.
% These two lists are then recursively sorted by quicksort, and joined
% together, with the pivot between them.

% Merge sort
% The list is split into two lists of (almost) equal length. These are
% then sorted separately and their results merged in order.

-module(sort).
-export([ quicksort/1, mergesort/1, merge/2 ]).
-include_lib("eunit/include/eunit.hrl").

splitbypivot([], _P, LE, G) ->
  { LE, G };
splitbypivot([ H | T ], P, LE, G) when H > P ->
  splitbypivot(T, P, LE, [ H | G ]);
splitbypivot([ H | T ], P, LE, G) ->
  splitbypivot(T, P, [ H | LE ], G ).

splitbypivot_test() ->
  { [ 4, 3, 2, 1 ], [ 6, 7, 8, 9 ] } = splitbypivot([ 1, 9, 2, 8, 3, 7, 4, 6 ], 5, [], []).

% just to avoid ++/2
concat([], B) ->
  B;
concat([ H | T ], B) ->
  [ H | concat(T, B) ].

concat_test() ->
  [] = concat([], []),
  [ 1, 2, 3, 4 ] = concat([ 1, 2, 3, 4 ], []),
  [ 1, 2, 3, 4 ] = concat([ 1, 2 ], [ 3, 4 ]),
  [ 1, 2, 3, 4 ] = concat([], [ 1, 2, 3, 4 ]).

% splitbypivot/4 is a substitution for list comprehensions
% which are not yet introduced in this chapter
quicksort([]) ->
  [];
quicksort([ H | T ]) ->
  { LE, G } = splitbypivot(T, H, [], []),
  concat(quicksort(LE), [ H | quicksort(G) ]).

quicksort_test() ->
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] = quicksort([ 8, 3, 5, 1, 2, 9, 7, 4, 6 ]).

splitbylen(Left, _, []) ->
  { Left, [] };
splitbylen(Left,  0,  L) ->
  { Left,   L };
splitbylen(Left, Len, [ H | T ]) ->
  splitbylen([ H | Left ], Len - 1, T).

splibylen_test() ->
  List5 = [ 7, 3, 6, 2, 5 ],
  { [ 3, 7 ], [ 6, 2, 5 ] } = splitbylen([], length(List5) div 2, List5),
  List6 = [ 4 | List5 ],
  { [ 3, 7, 4 ], [ 6, 2, 5 ] } = splitbylen([], length(List6) div 2, List6).

merge([], Right) ->
  Right;
merge(Left, []) ->
  Left;
merge([ L | LT ], [ R | RT ]) when R > L ->
  [ L | merge(LT, [ R | RT ]) ];
merge([ L | LT ], [ R | RT ]) ->
  [ R | merge([ L | LT ], RT) ].

merge_test() ->
  [ 1 ] = merge([], [ 1 ]),
  [ 1 ] = merge([ 1 ], []),
  [ 2, 3, 4, 5 ] = merge([ 2, 4 ], [ 3, 5 ]).

mergesort([]) ->
  [];
mergesort([ H ]) ->
  [ H ];
mergesort(L) ->
  { Left, Right } = splitbylen([], length(L) div 2, L),
  merge(mergesort(Left), mergesort(Right)).

mergesort_test() ->
  [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ] = mergesort([ 8, 3, 5, 1, 2, 9, 7, 4, 6 ]).
