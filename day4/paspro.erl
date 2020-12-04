-module(paspro).

-compile([export_all]).

-record(passport, 
        {
          byr,
          iyr,
          eyr,
          hgt,
          hcl,
          ecl,
          pid,
          cid
        }).

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

mod(X, Y) -> (X rem Y + Y) rem Y.

go(FileName) ->
  Keys = lists:flatmap(fun(Line) -> string:split(Line, " ", all) end, read_lines(FileName, none)),
  solve_first(Keys),
  solve_second(Keys).

solve_first(Keys) ->
  Passports = get_passports([], Keys),
  io:format("First: ~p~n", [length(lists:filter(fun(X) -> is_valid_passport(X) end, Passports))]),
  ok.

solve_second(Keys) ->
  Passports = get_passports([], Keys),
  io:format("Second: ~p~n", [length(lists:filter(fun(X) -> meets_extra_requirements(X) end, lists:filter(fun(X) -> is_valid_passport(X) end, Passports)))]),
  ok.

get_passports(L, [])   -> L;
get_passports(L, Keys) ->
  {P, Ks} = get_passport(#passport{}, Keys),
  get_passports([P | L], Ks).

get_passport(P, [])           -> {P, []};
get_passport(P, [""  | Keys]) -> {P, Keys};
get_passport(P, [Key | Keys]) ->
  [Field, Value] = string:split(Key, ":", all),
  get_passport(
    case list_to_atom(Field) of
      byr -> P#passport{byr = Value};
      iyr -> P#passport{iyr = Value};
      eyr -> P#passport{eyr = Value};
      hgt -> P#passport{hgt = Value};
      hcl -> P#passport{hcl = Value};
      ecl -> P#passport{ecl = Value};
      pid -> P#passport{pid = Value};
      cid -> P#passport{cid = Value}
    end, Keys).

is_valid_passport(#passport{byr = undefined}) -> false;
is_valid_passport(#passport{iyr = undefined}) -> false;
is_valid_passport(#passport{eyr = undefined}) -> false;
is_valid_passport(#passport{hgt = undefined}) -> false;
is_valid_passport(#passport{hcl = undefined}) -> false;
is_valid_passport(#passport{ecl = undefined}) -> false;
is_valid_passport(#passport{pid = undefined}) -> false;
is_valid_passport(_) -> true.

meets_extra_requirements(P) ->
  lists:all(
    fun(X) -> X end,
    [
     is_int_in_range(P#passport.byr, 1920, 2002, 4),
     is_int_in_range(P#passport.iyr, 2010, 2020, 4),
     is_int_in_range(P#passport.eyr, 2020, 2030, 4),
     is_valid_length(P#passport.hgt),
     is_hex_color(P#passport.hcl),
     lists:any(fun(X) -> X =:= list_to_atom(P#passport.ecl) end, [amb, blu, brn, gry, grn, hzl, oth]),
     is_int_in_range(P#passport.pid, 0, 999999999, 9)
    ]
   ).

is_valid_length(L) ->
  case re:run(L, "^[0-9]+cm$") of
    nomatch -> case re:run(L, "^[0-9]+in$") of
                 nomatch -> false;
                 _ -> [Nr | _] = string:replace(L, "in", ""),
                      is_int_in_range(Nr, 59, 76, 2)
               end;
    _ -> [Nr | _] = string:replace(L, "cm", ""),
         is_int_in_range(Nr, 150, 193, 3)
  end.



is_int_in_range(Int, L, U, Size) ->
  case (length(Int) == Size) of
    true -> case list_is_integer(Int) of
              true -> in_range(Int, L, U);
              false -> false
            end;
    false -> false
  end.

list_is_integer(L) when is_list(L) -> re:run(L, "^[0-9]+$") =/= nomatch.

in_range(X, L, U) -> 
  Xi = list_to_integer(X),
  (Xi >= L) and (Xi =< U).

is_hex_color(Hex) -> re:run(Hex, "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$") =/= nomatch.
