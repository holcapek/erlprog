% Write a function that returns a list of the format [1,2,..,N-1,N].

% Write a function that returns a list of the format [N, N-1,..,2,1].

-module(list).
-export([create/1, create_tail/1, rev_create/1, rev_create_tail/1]).

create(N, N) -> [ N ];
create(N, I) when I < N -> [ I | create(N, I + 1) ].

create(N) when N > 0 -> create(N, 1).

create_tail(1, A) -> [ 1 | A ];
create_tail(N, A) -> create_tail(N - 1, [ N | A ]).

create_tail(N) when N > 0 -> create_tail(N, []).

rev_create(1) -> [ 1 ];
rev_create(N) when N > 1 -> [ N | rev_create(N - 1) ].

rev_create_tail(N, N, A) -> [ N | A ];
rev_create_tail(N, I, A) when I < N -> rev_create_tail(N, I + 1, [ I | A ]).

rev_create_tail(N) when N > 0 -> rev_create_tail(N, 1, []).
