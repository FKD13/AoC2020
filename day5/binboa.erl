-module(binboa).

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
  Max = lists:max(calculate_seat_values(Lines)),
  io:format("First: ~p~n", [Max]),
  ok.

solve_second(Lines) ->
  Scores = calculate_seat_values(Lines),
  io:format("Second: ~p~n", [find_seat(lists:sort(Scores))]),
  ok.

find_seat([_])                        -> no_seat;
find_seat([F, S | _]) when F + 2 == S -> F + 1;
find_seat([_ | Rest])                 -> find_seat(Rest).

calculate_seat_values(Lines) -> lists:map(fun(X) -> score(X, "BR") end, Lines).

score([L], Keep) -> return_when_in_list(1, L, Keep);
score([L| Letters], Keep) -> return_when_in_list(math:pow(2, length(Letters)), L, Keep) + score(Letters, Keep).

return_when_in_list(Return, Letter, List) ->
  case lists:any(fun(X)-> Letter =:= X end, List) of
    true -> Return;
    _    -> 0
  end.
