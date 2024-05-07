-module(bech32).
-export([encode/2, encode/3, decode/1]).

-include_lib("eunit/include/eunit.hrl").

-define(ChecksumLength, 6).

-define(ErrNoHrp, no_hrp).
-define(ErrNoHrpSeparator, no_hrp_separator).
-define(ErrNoChecksum, no_checksum).
-define(ErrInvalidChecksum, invalid_checksum).

encode(Hrp, Data) when is_binary(Hrp), is_binary(Data) ->
  encode(Hrp, Data, lower).

encode(Hrp, Data, CharCase) when is_binary(Hrp), is_binary(Data), is_atom(CharCase) ->
  HrpList = binary_to_list(Hrp),
  case expand_hrp(HrpList) of
    {ok, ExpandedHrp} ->
      Base32Data = binary_to_base32(Data),
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

decode(Address) when is_binary(Address) ->
  AddressList = string:to_lower(binary_to_list(Address)),
  case extract_hrp(lists:reverse(AddressList), []) of
    {ok, Hrp, DataAndChecksum} ->
      case expand_hrp(Hrp) of
        {ok, ExpandedHrp} ->
          DecodedDataAndChecksum = lists:map(fun(X) -> value(X) end, DataAndChecksum),
          Values = lists:flatten([ExpandedHrp, DecodedDataAndChecksum]),
          case polymod(Values) == 1 of
            true ->
              {DecodedData, _} = lists:split(length(DecodedDataAndChecksum) - ?ChecksumLength, DecodedDataAndChecksum),
              {ok, list_to_binary(Hrp), base32_to_binary(DecodedData)};
            false ->
              {error, ?ErrInvalidChecksum}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

extract_hrp([], _) ->
  {error, ?ErrNoHrpSeparator};
extract_hrp([$1], _) ->
  {error, ?ErrNoHrp};
extract_hrp([$1 | Hrp], Data) when length(Data) >= ?ChecksumLength ->
  {ok, lists:reverse(Hrp), Data};
extract_hrp([$1 | _], _) ->
  {error, ?ErrNoChecksum};
extract_hrp([Ch | Hrp], Data) ->
  extract_hrp(Hrp, [Ch | Data]).

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
    X = [Gen || {Flag, Gen} <- Coefficients, Flag == 1],
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

value($q) -> 0;
value($p) -> 1;
value($z) -> 2;
value($r) -> 3;
value($y) -> 4;
value($9) -> 5;
value($x) -> 6;
value($8) -> 7;
value($g) -> 8;
value($f) -> 9;
value($2) -> 10;
value($t) -> 11;
value($v) -> 12;
value($d) -> 13;
value($w) -> 14;
value($0) -> 15;
value($s) -> 16;
value($3) -> 17;
value($j) -> 18;
value($n) -> 19;
value($5) -> 20;
value($4) -> 21;
value($k) -> 22;
value($h) -> 23;
value($c) -> 24;
value($e) -> 25;
value($6) -> 26;
value($m) -> 27;
value($u) -> 28;
value($a) -> 29;
value($7) -> 30;
value($l) -> 31.

binary_to_base32(Binary) when is_binary(Binary) ->
  bits_to_base32(binary_to_bits(Binary, []), []).

base32_to_binary(Base32) when is_list(Base32) ->
  bits_to_binary(base32_to_bits(Base32, []), []).

base32_to_bits([], Bits) ->
  lists:reverse(Bits);
base32_to_bits([B | Base32], Bits) ->
  <<_:3, B4:1, B3: 1, B2:1, B1:1, B0:1>> = <<B:8>>,
  base32_to_bits(Base32, [B0, B1, B2, B3, B4 | Bits]).

bits_to_base32([], Base32List) ->
  lists:reverse(Base32List);
bits_to_base32([B4], Base32List) ->
  lists:reverse([(B4 bsl 4) | Base32List]);
bits_to_base32([B4, B3], Base32List) ->
  lists:reverse([(B4 bsl 4) bor (B3 bsl 3) | Base32List]);
bits_to_base32([B4, B3, B2], Base32List) ->
  lists:reverse([(B4 bsl 4) bor (B3 bsl 3) bor (B2 bsl 2) | Base32List]);
bits_to_base32([B4, B3, B2, B1], Base32List) ->
  lists:reverse([(B4 bsl 4) bor (B3 bsl 3) bor (B2 bsl 2) bor (B1 bsl 1) | Base32List]);
bits_to_base32([B4, B3, B2, B1, B0 | Bits], Base32List) ->
  bits_to_base32(Bits, [(B4 bsl 4) bor (B3 bsl 3) bor (B2 bsl 2) bor (B1 bsl 1) bor B0 | Base32List]).

binary_to_bits(<<>>, Bits) ->
  lists:reverse(Bits);
binary_to_bits(<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1, Rest/binary>>, Bits) ->
  binary_to_bits(Rest, [B0, B1, B2, B3, B4, B5, B6, B7 | Bits]).

bits_to_binary([], Bytes) ->
  list_to_binary(lists:reverse(Bytes));
bits_to_binary([B7], Bytes) ->
  bits_to_binary([], [<<B7:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1>> | Bytes]);
bits_to_binary([B7, B6], Bytes) ->
  bits_to_binary([], [<<B7:1, B6:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1>> | Bytes]);
bits_to_binary([B7, B6, B5], Bytes) ->
  bits_to_binary([], [<<B7:1, B6:1, B5:1, 0:1, 0:1, 0:1, 0:1, 0:1>> | Bytes]);
bits_to_binary([B7, B6, B5, B4], Bytes) ->
  bits_to_binary([], [<<B7:1, B6:1, B5:1, B4:1, 0:1, 0:1, 0:1, 0:1>> | Bytes]);
bits_to_binary([B7, B6, B5, B4, B3], Bytes) ->
  bits_to_binary([], [<<B7:1, B6:1, B5:1, B4:1, B3:1, 0:1, 0:1, 0:1>> | Bytes]);
bits_to_binary([B7, B6, B5, B4, B3, B2], Bytes) ->
  bits_to_binary([], [<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, 0:1, 0:1>> | Bytes]);
bits_to_binary([B7, B6, B5, B4, B3, B2, B1], Bytes) ->
  bits_to_binary([], [<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, 0:1>> | Bytes]);
bits_to_binary([B7, B6, B5, B4, B3, B2, B1, B0], Bytes) ->
  bits_to_binary([], [<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>> | Bytes]);
bits_to_binary([B7, B6, B5, B4, B3, B2, B1, B0 | Bits], Bytes) ->
  bits_to_binary(Bits, [<<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>> | Bytes]).

%=======================================================================================================================
% Unit tests
%=======================================================================================================================

char_test_() ->
  Actual = lists:foldr(fun(X, Acc) -> Char = char(X), [Char | Acc] end, [], lists:seq(0, 31)),
  [
    ?_assertEqual("qpzry9x8gf2tvdw0s3jn54khce6mua7l", Actual),
    ?_assertEqual("QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7L", string:to_upper(Actual))
  ].
