% Implement the database-handling list in Exercise 3-4 using the lists
% module library functions. Maintain the same interface to the db module,
% allowing your two modules to be interchangeable.

-module(dblists).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2 ]).
-include_lib("eunit/include/eunit.hrl").

new() -> [].

destroy(_Db) -> ok.

% unfortunately, lists:keystore(Key, 1, Db, { Key, Val })
% would append the tuple at the end of the list
write(Key, Val, Db) ->
  [ { Key, Val } | delete(Key, Db) ].

delete(Key, Db) ->
  lists:keydelete(Key, 1, Db).

read(Key, Db) ->
  case lists:keyfind(Key, 1, Db) of
    { Key, Val } -> { ok, Val };
    false -> {error, instance}
  end.

match(Val, Db) ->
  lists:filtermap(
    fun({ K, V }) ->
      case V == Val of
        true -> { true, K };
        false -> false
      end
    end,
    Db
  ).

dblists_test() ->
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
  ok = destroy(Db7).


