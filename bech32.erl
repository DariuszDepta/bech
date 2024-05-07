-module(bech32).
-export([encode/2, encode/3, decode/1]).

-include_lib("eunit/include/eunit.hrl").

encode(Hrp, Data) when is_binary(Hrp), is_binary(Data) ->
  encode(Hrp, Data, lower).

encode(Hrp, Data, CharCase) when is_binary(Hrp), is_binary(Data), is_atom(CharCase) ->
  HrpList = binary_to_list(Hrp),
  case expand_hrp(HrpList) of
    {ok, ExpandedHrp} ->
      Base32Data = base32(binary_to_bits(Data, []), []),
      Values = lists:flatten([ExpandedHrp, Base32Data, [0, 0, 0, 0, 0, 0]]),
      PolyMod = polymod(Values) bxor 1,
      Checksum = lists:map(fun(X) -> char((PolyMod bsr (5 * (5 - X))) band 16#1F) end, [0, 1, 2, 3, 4, 5]),
      EncodedData = lists:map(fun(X) -> char(X) end, Base32Data),
      Encoded = char_case(lists:flatten([HrpList, [$1], EncodedData, Checksum]), CharCase),
      case length(Encoded) =< 90 of
        true ->
          {ok, list_to_binary(Encoded)};
        false ->
          {error, too_long}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

decode(_Address) ->
  decode.

char_case(List, lower) ->
  string:to_lower(List);
char_case(List, upper) ->
  string:to_upper(List).

expand_hrp([]) ->
  {error, no_hrp};
expand_hrp(Hrp) when length(Hrp) > 83 ->
  {error, too_long_hrp};
expand_hrp(Hrp) when is_list(Hrp) ->
  case lists:all(fun(Ch) -> (Ch >= 33) and (Ch =< 126) end, Hrp) of
    true ->
      UpperPart = lists:map(fun(C) -> C bsr 5 end, Hrp),
      LowerPart = lists:map(fun(C) -> C band 16#1F end, Hrp),
      {ok, lists:flatten([UpperPart, [0], LowerPart])};
    false ->
      {error, invalid_hrp_character}
  end.

polymod(Values) when is_list(Values) ->
  F = fun(Value, Chk) ->
    B = Chk bsr 25,
    Chk1 = ((Chk band 16#1FFFFFF) bsl 5) bxor Value,
    <<_:3, B4:1, B3:1, B2:1, B1:1, B0:1>> = <<B:8>>,
    Coefficients = lists:zip([B0, B1, B2, B3, B4], [16#3b6a57b2, 16#26508e6d, 16#1ea119fa, 16#3d4233dd, 16#2a1462b3]),
    X = [Gen || {Flag, Gen} <- Coefficients, Flag =:= 1],
    lists:foldl(fun(Gen, Acc) -> Acc bxor Gen end, Chk1, X)
  end,
  lists:foldl(F, 1, Values).

char(0) -> $q;
char(1) -> $p;
char(2) -> $z;
char(3) -> $r;
char(4) -> $y;
char(5) -> $9;
char(6) -> $x;
char(7) -> $8;
char(8) -> $g;
char(9) -> $f;
char(10) -> $2;
char(11) -> $t;
char(12) -> $v;
char(13) -> $d;
char(14) -> $w;
char(15) -> $0;
char(16) -> $s;
char(17) -> $3;
char(18) -> $j;
char(19) -> $n;
char(20) -> $5;
char(21) -> $4;
char(22) -> $k;
char(23) -> $h;
char(24) -> $c;
char(25) -> $e;
char(26) -> $6;
char(27) -> $m;
char(28) -> $u;
char(29) -> $a;
char(30) -> $7;
char(31) -> $l.

binary_to_bits(<<>>, Bits) ->
  lists:reverse(Bits);
binary_to_bits(<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1, Rest/binary>>, Bits) ->
  binary_to_bits(Rest, [B0, B1, B2, B3, B4, B5, B6, B7 | Bits]).

base32([], List) ->
  lists:reverse(List);
base32([B4], List) ->
  lists:reverse([(B4 bsl 4) | List]);
base32([B4, B3], List) ->
  lists:reverse([(B4 bsl 4) bor (B3 bsl 3) | List]);
base32([B4, B3, B2], List) ->
  lists:reverse([(B4 bsl 4) bor (B3 bsl 3) bor (B2 bsl 2) | List]);
base32([B4, B3, B2, B1], List) ->
  lists:reverse([(B4 bsl 4) bor (B3 bsl 3) bor (B2 bsl 2) bor (B1 bsl 1) | List]);
base32([B4, B3, B2, B1, B0 | Tail], List) ->
  base32(Tail, [(B4 bsl 4) bor (B3 bsl 3) bor (B2 bsl 2) bor (B1 bsl 1) bor B0 | List]).

%=======================================================================================================================
% Unit tests
%=======================================================================================================================

char_test_() ->
  Actual = lists:foldr(fun(X, Acc) -> Char = char(X), [Char | Acc] end, [], lists:seq(0, 31)),
  [
    ?_assertEqual("qpzry9x8gf2tvdw0s3jn54khce6mua7l", Actual),
    ?_assertEqual("QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7L", string:to_upper(Actual))
  ].
