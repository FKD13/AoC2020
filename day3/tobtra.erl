-module(tobtra).

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
  Forest = read_lines(IoDev),
  io:format("Answer1: ~p~n", [init_desend(3, Forest)]),
  io:format("Answer2: ~p~n", 
    [
      init_desend(1, Forest) *
      init_desend(3, Forest) *
      init_desend(5, Forest) *
      init_desend(7, Forest) * 
      init_desend(1, cutdown_oneven_trees(0, Forest))
    ]).

cutdown_oneven_trees(_, []) -> [];
cutdown_oneven_trees(N, [T|Forest]) when (N band 1) == 0 -> [T|cutdown_oneven_trees(N+1, Forest)];
cutdown_oneven_trees(N, [_|Forest]) -> cutdown_oneven_trees(N+1, Forest).

init_desend(Rate, Forest) -> desend(1+Rate, length(hd(Forest))+1, Rate, Forest).

desend(_, _, _, [_ |[]]) -> 0;
desend(X, Width, Rate, F) when X >= Width -> desend(mod(X, Width)+1, Width, Rate, F);
desend(X, Width, Rate, [_, Next | Forest]) ->
  case lists:nth(X, Next) of
    $# -> 1 + desend(X + Rate, Width, Rate, [Next | Forest]);
    $. -> desend(X + Rate, Width, Rate, [Next | Forest])
  end.

mod(X, Y) -> (X rem Y + Y) rem Y.

