% Write a module db.erl that creates a database and is able to store, retrieve,
% and delete elements in it. ... You may not use the lists library module,
% and you have to implement all the recursive functions yourself.
%
% Interface:
% db:new() -> Db.
% db:destroy(Db) -> ok.
% db:write(Key, Element, Db) -> NewDb.
% db:delete(Key, Db) -> NewDb.
% db:read(Key, Db) -> {ok, Element} | {error, instance}.
% ^^^ Based on read/2 only returns single element (in the tuple),
%     I take it that write/3 overwrites element of an existing key
% db:match(Element, Db) -> [Key1, ..., KeyN].
%
% Hint: use lists and tuples as your main data structures.

-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2 ]).
-include_lib("eunit/include/eunit.hrl").

new() -> [].

destroy(_Db) -> ok.

write(Key, Val, T) ->
  [ { Key, Val } | delete(Key, T) ].

delete(_Key, []) ->
  [];
delete(Key, [ { Key, _Val } | T ]) ->
  T;
delete(Key, [ H | T ]) ->
  [ H | delete(Key, T) ].

read(_Key, []) ->
  { error, instance };
read(Key, [ { Key, Val } | _T ]) ->
  { ok, Val };
read(Key, [ _H | T ]) ->
  read(Key, T).

match(_Val, []) ->
  [];
match(Val, [ { Key, Val } | T ]) ->
  [ Key | match(Val, T) ];
match(Val, [ _H | T ]) ->
  match(Val, T).

db_test() ->
  Db1 = new(),
  [] = Db1,
  Db2 = write(francesco, london, Db1),
  [ { francesco, london } ] = Db2,
  Db3 = write(lelle, stockholm, Db2),
  [ { lelle, stockholm }, { francesco, london } ] = Db3,
  { ok, london } = read(francesco, Db3),
  Db4 = write(joern, stockholm, Db3),
  [ { joern, stockholm }, { lelle, stockholm }, { francesco, london } ] = Db4,
  { error, instance } = read(ola, Db4),
  [ joern, lelle ] = match(stockholm, Db4),
  Db5 = delete(lelle, Db4),
  [ { joern, stockholm }, { francesco, london } ] = Db5,
  [ joern ] = match(stockholm, Db5),
  Db6 = delete(francesco, Db5),
  [ { joern, stockholm } ] = Db6,
  Db7 = delete(joern, Db6),
  [] = Db7,
  ok = destroy([]).


