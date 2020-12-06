-module(cuscus).

-compile([export_all]).

read_lines(FileName, none) -> {ok, IoDev} = file:open(FileName, read), read_lines(FileName, IoDev);
read_lines(FileName, IoDev) ->
  Res = file:read_line(IoDev),
  case Res of
    {ok, Line} -> [string:trim(Line) | read_lines(FileName, IoDev)];
    eof -> file:close(IoDev), [];
    {error, Reason} -> 
      io:format("Error: ~p~n", [Reason]),
      exit(1)
  end.

go(FileName) ->
  Lines = read_lines(FileName, none),
  solve_first(Lines),
  solve_second(Lines).

solve_first(Lines) ->
  Value = lists:sum(lists:map(fun(X) -> length(lists:usort(X)) end, concat_strings([], Lines))),
  io:format("First: ~p~n", [Value]),
  ok.

solve_second(Lines) ->
  Value = lists:sum(lists:map(fun(X) -> length(lists:usort(sets:to_list(X))) end, concat_strings2([], Lines))),
  io:format("Second: ~p~n", [Value]),
  ok.

concat_strings(S       , []       ) -> S;
concat_strings([]      , Lines    ) -> concat_strings([""], Lines);
concat_strings(S       , ["" | Xl]) -> concat_strings(["" | S], Xl);
concat_strings([S | Xs], [L  | Xl]) -> concat_strings([string:concat(S, L) | Xs], Xl).

concat_strings2(S       , []       ) -> S;
concat_strings2([]      , Lines    ) -> concat_strings2([sets:from_list("abcdefghijklnmopqrstuvwxyz")], Lines);
concat_strings2(S       , ["" | Xl]) -> concat_strings2([sets:from_list("abcdefghijklnmopqrstuvwxyz") | S], Xl);
concat_strings2([S | Xs], [L  | Xl]) -> concat_strings2([sets:intersection(S, sets:from_list(L)) | Xs], Xl).
