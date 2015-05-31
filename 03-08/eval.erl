% This exercise asks you to build a collection of functions that
% manipulate arithmetical expressions. Start with an expression
% such as the following:
%
% ((2+3)-4)
% 4
% ~((2*3)+(3*4))
%
% which is fully bracketed and where you use a tilde (~) for unary
% minus.
%
% First, write a parser for these, turning them into Erlang
% representations, such as the following:
%
% {minus, {plus, {num, 2}, {num,3}}, {num, 4}}
%
% which represents ((2+3)-4). We call these exps. Now, write a number
% of functions:
%
% An evaluator, which takes an exp and returns its value
%
% A pretty printer, which will turn an exp into a string representation
%
% A compiler, which transforms an exp into a sequence of code for a stack machine
% to evaluate the exp
%
% A simulator which will implement expressions for the stack machine
%
% A simplifier, which will simplify an expression so that 0*e is transformed to 0 , 1*e to
% e , and so on (there are quite a lot of others to think of!)
%
%
% You can also extend the collection of expressions to add conditionals:
%
% if ((2+3)-4) then 4 else ~((2*3)+(3*4))
%
% where the value returned is the “then” value if the “if” expression evaluates to 0 , and
% it is the “else” value otherwise.
%
% You could also add local definitions, such as the following:
%
% let c = ((2+3)-4) in ~((2*c)+(3*4))
%
% Or you could add variables, which are set and then used in subsequent expressions.

-module(eval).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

tokenize([]) -> [];
% white space
tokenize([ $  | T ]) -> tokenize(T);
tokenize([ $+ | T ]) -> [ '+' | tokenize(T) ];
tokenize([ $- | T ]) -> [ '-' | tokenize(T) ];
tokenize([ $* | T ]) -> [ '*' | tokenize(T) ];
tokenize([ $/ | T ]) -> [ '/' | tokenize(T) ];
tokenize([ $~ | T ]) -> [ '~' | tokenize(T) ];
tokenize([ $( | T ]) -> [ '(' | tokenize(T) ];
tokenize([ $) | T ]) -> [ ')' | tokenize(T) ];
% number
tokenize([ N | T ]) when N >= $0, N =< $9 ->
  tokenize_num(T, N - $0).

tokenize_num([ H | T ], N) when H >= $0, H =< $9 ->
  tokenize_num(T, N * 10 + H - $0);
tokenize_num([ H | T ], N) when H == $. ->
  tokenize_float(T, N, 10);
tokenize_num(T, N) -> [ {num, N} | tokenize(T) ].

tokenize_float([ H | T ], N, E) when H >= $0, H =< $9 ->
  tokenize_float(T, N + (H - $0)/E, E * 10);
tokenize_float(T, N, _) -> [ {num, N} | tokenize(T) ].

tokenize_test() ->
  [ '~', '(', '(', {num, 2}, '*', {num, 3}, ')',
    '+', '(', {num, 3}, '*', {num, 4}, ')', ')' ] = tokenize("~((2*3)+(3*4))").

parse2([ {num, N} | T ]) ->
  { {num, N}, T };
parse2([ '~' | T ]) ->
  { E, T1 } = parse2(T),
  { { '~', E }, T1 };
parse2([ '(' | T ]) ->
  { E, [ ')' | T1 ] } = parse2_binop(T),
  { E, T1 }.

parse2_binop(L) ->
  { E1, [ Op | T ] } = parse2(L),
  true = case Op of
    '+' -> true;
    '-' -> true;
    '*' -> true;
    '/' -> true
  end,
  { E2, T1 } = parse2(T),
  { { Op, E1, E2 }, T1 }.

parse(L) ->
  { E, [] } = parse2(L),
  E.
