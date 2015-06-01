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
% TODO You could also add local definitions, such as the following:
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
tokenize("if"   ++ T)  -> [ 'if'   | tokenize(T) ];
tokenize("then" ++ T)  -> [ 'then' | tokenize(T) ];
tokenize("else" ++ T)  -> [ 'else' | tokenize(T) ];
% number
tokenize([ N | T ]) when N >= $0, N =< $9 ->
  tokenize_num(T, N - $0).

tokenize_num([ H | T ], N) when H >= $0, H =< $9 ->
  tokenize_num(T, N * 10 + H - $0);
tokenize_num([ $. | T ], N) ->
  tokenize_float(T, N, 10);
tokenize_num(T, N) -> [ {num, N} | tokenize(T) ].

tokenize_float([ H | T ], N, E) when H >= $0, H =< $9 ->
  tokenize_float(T, N + (H - $0)/E, E * 10);
tokenize_float(T, N, _) ->
  [ {num, N} | tokenize(T) ].

tokenize_test() ->
  [ '~', '(', '(', {num, 2}, '+', {num, 3}, ')',
    '+', '(', {num, 3}, '*', {num, 4}, ')', ')' ] 
    = tokenize("~((2+3)+(3*4))"),
  [ 'if', '(', '(', {num, 2}, '+', {num, 3}, ')', '-', {num,4}, ')', 'then', {num, 4}, 'else',
    '~', '(', '(', {num, 2}, '*', {num, 3}, ')', '+', '(', {num, 3}, '*', {num, 4}, ')', ')' ]
    = tokenize("if ((2+3)-4) then 4 else ~((2*3)+(3*4))").

parse2([ {num, N} | T ]) ->
  { {num, N}, T };
parse2([ '~' | T ]) ->
  { E, T1 } = parse2(T),
  { { '~', E }, T1 };
parse2([ '(' | T ]) ->
  { E, [ ')' | T1 ] } = parse2_binop(T),
  { E, T1 };
parse2([ 'if' | T ]) ->
  { C,  [ 'then' | T1 ] } = parse2(T),
  { E1, [ 'else' | T2 ] } = parse2(T1),
  { E2, T3 } = parse2(T2),
  { { 'if', C, E1, E2 }, T3 }.

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

parse_test() ->
  { '~', { '+', { '*', {num, 2}, {num, 3} },
  { '*', { num, 3 }, {num, 4 } } } }
    = parse(tokenize("~((2*3)+(3*4))")),
  { 'if', { '-', { '+', { num, 2}, {num, 3} }, {num, 4} },
    {num, 4},
    { '~', { '+', { '*', {num, 2}, {num, 3} }, { '*', {num, 3}, {num,4 } } } } }
    = parse(tokenize("if ((2+3)-4) then 4 else ~((2*3)+(3*4))")).

eval({ '~', E }) ->
  R = eval(E),
  -R;
eval({ num, N }) ->
  N;
eval({ '+', E1, E2 }) ->
  R1 = eval(E1),
  R2 = eval(E2),
  R1 + R2;
eval({ '-', E1, E2 }) ->
  R1 = eval(E1),
  R2 = eval(E2),
  R1 - R2;
eval({ '*', E1, E2 }) ->
  R1 = eval(E1),
  R2 = eval(E2),
  R1 * R2;
eval({ '/', E1, E2 }) ->
  R1 = eval(E1),
  R2 = eval(E2),
  R1 / R2;
eval({ 'if', C, E1, E2 }) ->
  C1 = eval(C),
  case C1 of
    0 -> eval(E1);
    _ -> eval(E2)
  end.

eval_test() ->
  1/12 = eval(parse(tokenize("~((1-2)/(3*4))"))),
  -18 = eval(parse(tokenize("if ((2+3)-4) then 4 else ~((2*3)+(3*4))"))).

print2({ '~', E }) ->
  [ $~ ] ++ print2(E);
print2({ num, N }) ->
  % just returning N would cause the list not to be recognized
  % as a pritable string
  io_lib:write(N);
print2({ '+', E1, E2 }) ->
  [ $(, print2(E1), $+, print2(E2), $) ];
print2({ '-', E1, E2 }) ->
  [ $(, print2(E1), $-, print2(E2), $) ];
print2({ '*', E1, E2 }) ->
  [ $(, print2(E1), $*, print2(E2), $) ];
print2({ '/', E1, E2 }) ->
  [ $(, print2(E1), $/, print2(E2), $) ];
print2({ 'if', C, E1, E2 }) ->
  [ "if ", print2(C), " then ", print2(E1), " else ", print2(E2) ].

print(E) ->
  lists:flatten(print2(E)).

print_test() ->
  "~((1-2)/(3*4))" = print(parse(tokenize("~((1-2)/(3*4))"))),
  "if ((2+3)-4) then 4 else ~((2*3)+(3*4))"
    = print(parse(tokenize("if ((2+3)-4) then 4 else ~((2*3)+(3*4))"))).

compile2({ '~', E }) ->
  [ compile2(E), '~' ];
compile2({ num, N }) ->
  [ N ];
compile2({ Op, E1, E2 }) ->
  [ compile2(E1), compile2(E2), Op ];
compile2({ 'if', C, E1, E2 }) ->
  [ compile2(E1), compile2(E2), compile(C), 'if' ].

compile(E) ->
  lists:flatten(compile2(E)).

compile_test() ->
  [ 1, 2, '-', 3, 4, '*', '/', '~' ]
    = compile(parse(tokenize("~((1-2)/(3*4))"))),
  [ 3, 4, '*', 5, 6, '/', 1, 2, '+', 'if' ]
    = compile(parse(tokenize("if (1+2) then (3*4) else (5/6)"))).

simulate([], [ N ])
when is_number(N) ->
  N;
simulate([ N | IT ], OT)
when is_number(N) ->
  simulate(IT, [ N | OT ]);
simulate([ '~' | IT ], [ N | OT ])
when is_number(N) ->
  simulate(IT, [ -N | OT ]);
simulate([ '+' | IT ], [ N1, N2 | OT ])
when is_number(N1), is_number(N2) ->
  simulate(IT, [ N2 + N1 | OT ]);
simulate([ '-' | IT ], [ N1, N2 | OT ])
when is_number(N1), is_number(N2) ->
  simulate(IT, [ N2 - N1 | OT ]);
simulate([ '*' | IT ], [ N1, N2 | OT ])
when is_number(N1), is_number(N2) ->
  simulate(IT, [ N2 * N1 | OT ]);
simulate([ '/' | IT ], [ N1, N2 | OT ])
when is_number(N1), is_number(N2) ->
  simulate(IT, [ N2 / N1 | OT ]);
simulate([ 'if' | IT ], [ C, E2, E1 | OT ]) ->
  case C of
    0 -> simulate(IT, [ E1 |OT ]);
    _ -> simulate(IT, [ E2 |OT ])
  end.

simulate(L) -> simulate(L, []).

simulate_test() ->
  1/12 = simulate(compile(parse(tokenize("~((1-2)/(3*4))")))),
  5/6 = simulate(compile(parse(tokenize("if (1+2) then (3*4) else (5/6)")))).

simplify({ num, N }) -> { num, N };
simplify({ '+', { num, N }, { num, 0 } }) -> { num, N };
simplify({ '+', { num, 0 }, { num, N } }) -> { num, N };
simplify({ '-', { num, N }, { num, N } }) -> { num, 0 };
simplify({ '*', { num, N }, { num, 1 } }) -> { num, N };
simplify({ '*', { num, 1 }, { num, N } }) -> { num, N };
simplify({ '*', { num, 0 }, { num, _ } }) -> { num, 0 };
simplify({ '*', { num, _ }, { num, 0 } }) -> { num, 0 };
simplify({ '/', { num, 0 }, { num, _ } }) -> { num, 0 };
simplify({ '~', E }) -> { '~', simplify(E) };
simplify({ Op, E1, E2 })
when Op == '+'; Op == '-'; Op == '*'; Op == '/' ->
  E3 = { Op, simplify(E1), simplify(E2) },
  simplify(E3).

simplify_test() ->
  { num, 0 } = simplify(parse(tokenize("((0+1)-(1+0))"))),
  { num, 0 } = simplify(parse(tokenize("((3*0)/(3*1))"))),
  { num, 0 } = simplify(parse(tokenize("((0*3)/(1*3))"))).
