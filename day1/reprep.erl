-module(reprep).

-export([go/1, read_lines/1]).

read_lines(IoDev) ->
  Res = file:read_line(IoDev),
  case Res of
    {ok, Line} -> [Line | read_lines(IoDev)];
    eof -> [];
    {error, Reason} -> 
      io:format("Error: ~p~n", [Reason]),
      exit(1)
  end.

go(FileName) ->
  {_, IoDev} = file:open(FileName, read),
  Lines = read_lines(IoDev),
  io:format("~p~n", [Lines]),
  Ints = [list_to_integer(string:trim(S)) || S <- Lines],
  [{X, Y, W}| _] = lists:filter(fun(W) -> not is_atom(W) end,
    lists:map(
    fun({X, Y, W}) ->
        Z = X + Y + W,
        case Z of
          2020 -> {X, Y, W};
          _ -> no
        end
    end,
    [{X, Y, W} || X <- Ints, Y <- Ints, W <- Ints]
  )),
  io:format("Found: ~p~n", [X * Y * W]).

