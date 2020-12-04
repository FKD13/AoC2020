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
  io:format("Answer1: ~p~n", [init_descend(3, Forest)]),
  io:format("Answer2: ~p~n", 
    [
      lists:foldl(fun(V, Acc) -> init_descend(V, Forest) * Acc end, 1, [1, 3, 5, 7]) *
      init_descend(1, cutdown_odd_trees(0, Forest))
    ]).

cutdown_odd_trees(_, []) -> [];
cutdown_odd_trees(N, [T|Forest]) when (N band 1) == 0 -> [T|cutdown_odd_trees(N+1, Forest)];
cutdown_odd_trees(N, [_|Forest]) -> cutdown_odd_trees(N+1, Forest).

init_descend(Rate, Forest) -> descend(1+Rate, length(hd(Forest))+1, Rate, Forest).

descend(_, _, _, [_ |[]]) -> 0;
descend(X, Width, R, F) when X >= Width -> descend(mod(X, Width)+1, Width, R, F);
descend(X, Width, Rate, [_, Next | Forest]) ->
  case lists:nth(X, Next) of
    $# -> 1 + descend(X + Rate, Width, Rate, [Next | Forest]);
    $. -> descend(X + Rate, Width, Rate, [Next | Forest])
  end.

mod(X, Y) -> (X rem Y + Y) rem Y.

