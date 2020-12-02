-module(pasphi).

-compile([export_all]).

read_lines(IoDev) ->
  Res = file:read_line(IoDev),
  case Res of
    {ok, Line} -> [string:trim(Line) | read_lines(IoDev)];
    eof -> [];
    {error, Reason} -> 
      io:format("Error: ~p~n", [Reason]),
      exit(1)
  end.

go(FileName) ->
  {_, IoDev} = file:open(FileName, read),
  Lines = [{[list_to_integer(X) || X <- string:tokens(Bounds, "-")], string:strip(Letter, both, $:), Password} || [Bounds, Letter, Password] <- [string:split(Line, " ", all) || Line <- read_lines(IoDev)]],
  io:format("~p~n", [Lines]),
  Correct = lists:filter(fun({[Lower, Upper], [Letter], Password}) -> is_valid2(Lower, Upper, Letter, Password) end, Lines),
  io:format("Correct ~p~n", [length(Correct)]).
  
is_valid(Lower, Upper, Letter, Password) ->
  Count = length([C || C <- Password, C =:= Letter]),
  (Lower =< Count) and (Count =< Upper).

is_valid2(Lower, Upper, Letter, Password) ->
  (Letter =:= lists:nth(Lower, Password)) xor (lists:nth(Upper, Password) =:= Letter).
