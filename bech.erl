-module(bech).
-export([encode/2, encode/3, decode/1]).

-define(HrpSeparator, $1).
-define(ChecksumLength, 6).
-define(MaxHrpLength, 83).
-define(MaxTotalLength, 90).

-define(ErrNoHrp, no_hrp).
-define(ErrNoHrpSeparator, no_hrp_separator).
-define(ErrHrpLengthExceeded, hrp_length_exceeded).
-define(ErrTooShortChecksum, too_short_checksum).
-define(ErrInvalidChecksum, invalid_checksum).
-define(ErrInvalidCharacter, invalid_character).
-define(ErrTotalLengthExceeded, total_length_exceeded).

%%% ============================================================================
%%%  Public functions
%%% ============================================================================

% Generates human readable address in Bech32 format, basing on provided
% prefix and data; all characters are lowercase.
%
encode(Hrp, Data) when is_binary(Hrp), is_binary(Data) ->
  encode(Hrp, Data, lower).

% Generates human readable address in Bech32 format, basing on provided
% prefix and data. All output characters are either lowercase or uppercase,
% depending on provided options.
%
encode(Hrp, Data, CharCase) when is_binary(Hrp), is_binary(Data), is_atom(CharCase) ->
  HrpList = binary_to_list(Hrp),
  case expand_hrp(HrpList) of
    {ok, ExpandedHrp} ->
      Base32Data = binary_to_base32(Data),
      Values = lists:flatten([ExpandedHrp, Base32Data, [0, 0, 0, 0, 0, 0]]),
      PolyMod = polymod(Values) bxor 1,
      Checksum = lists:map(fun(X) -> char((PolyMod bsr (5 * (5 - X))) band 16#1F) end, [0, 1, 2, 3, 4, 5]),
      EncodedData = lists:map(fun(X) -> char(X) end, Base32Data),
      Encoded = char_case(lists:flatten([HrpList, [?HrpSeparator], EncodedData, Checksum]), CharCase),
      case length(Encoded) =< ?MaxTotalLength of
        true ->
          {ok, list_to_binary(Encoded)};
        false ->
          {error, ?ErrTotalLengthExceeded, length(Encoded)}
      end;
    Error -> Error
  end.

% Decodes content from specified human readable address in Bech32 format.
% Extracts prefix and data, verifies the checksum.
%
-spec decode(Input::binary()) -> {ok, Hrp::binary(), Data::binary()} | {error, Reason::atom()} | {error, Reason::atom(), Value::any()}.
decode(Input) when is_binary(Input) ->
  InputList = string:to_lower(binary_to_list(Input)),
  case length(InputList) =< ?MaxTotalLength of
    true ->
      case extract_hrp(lists:reverse(InputList), []) of
        {ok, Hrp, DataAndChecksum} ->
          case expand_hrp(Hrp) of
            {ok, ExpandedHrp} ->
              case characters_to_base32(DataAndChecksum, []) of
                {ok, DecodedData} ->
                  Values = lists:flatten([ExpandedHrp, DecodedData]),
                  case polymod(Values) == 1 of
                    true ->
                      {Decoded, _} = lists:split(length(DecodedData) - ?ChecksumLength, DecodedData),
                      {ok, list_to_binary(Hrp), base32_to_binary(Decoded)};
                    false ->
                      {error, ?ErrInvalidChecksum}
                  end;
                Other ->
                  Other
              end;
            Error -> Error
          end;
        Error -> Error
      end;
    false ->
      {error, ?ErrTotalLengthExceeded, length(InputList)}
  end.

%%% ============================================================================
%%%  Private functions
%%% ============================================================================

extract_hrp([], _) ->
  {error, ?ErrNoHrpSeparator};
extract_hrp([?HrpSeparator], _) ->
  {error, ?ErrNoHrp};
extract_hrp([?HrpSeparator | Hrp], Data) when length(Data) >= ?ChecksumLength ->
  {ok, lists:reverse(Hrp), Data};
extract_hrp([?HrpSeparator | _], _) ->
  {error, ?ErrTooShortChecksum};
extract_hrp([Ch | Hrp], Data) ->
  extract_hrp(Hrp, [Ch | Data]).

char_case(List, upper) ->
  string:to_upper(List);
char_case(List, _) ->
  List.

expand_hrp([]) ->
  {error, ?ErrNoHrp};
expand_hrp(Hrp) when length(Hrp) > ?MaxHrpLength ->
  {error, ?ErrHrpLengthExceeded, length(Hrp)};
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

value($q) -> {ok, 0};
value($p) -> {ok, 1};
value($z) -> {ok, 2};
value($r) -> {ok, 3};
value($y) -> {ok, 4};
value($9) -> {ok, 5};
value($x) -> {ok, 6};
value($8) -> {ok, 7};
value($g) -> {ok, 8};
value($f) -> {ok, 9};
value($2) -> {ok, 10};
value($t) -> {ok, 11};
value($v) -> {ok, 12};
value($d) -> {ok, 13};
value($w) -> {ok, 14};
value($0) -> {ok, 15};
value($s) -> {ok, 16};
value($3) -> {ok, 17};
value($j) -> {ok, 18};
value($n) -> {ok, 19};
value($5) -> {ok, 20};
value($4) -> {ok, 21};
value($k) -> {ok, 22};
value($h) -> {ok, 23};
value($c) -> {ok, 24};
value($e) -> {ok, 25};
value($6) -> {ok, 26};
value($m) -> {ok, 27};
value($u) -> {ok, 28};
value($a) -> {ok, 29};
value($7) -> {ok, 30};
value($l) -> {ok, 31};
value(Ch) -> {error, ?ErrInvalidCharacter, Ch}.

characters_to_base32([], Base32) ->
  {ok, lists:reverse(Base32)};
characters_to_base32([Ch | Tail], Base32) ->
  case value(Ch) of
    {ok, Value} ->
      characters_to_base32(Tail, [Value | Base32]);
    {error, Reason, Ch} ->
      {error, Reason, Ch}
  end.

binary_to_base32(Binary) when is_binary(Binary) ->
  bits_to_base32(binary_to_bits(Binary, []), []).

base32_to_binary(Base32) when is_list(Base32) ->
  Bits = base32_to_bits(Base32, []),
  BitsLength = length(Bits),
  ToDrop = BitsLength rem 8,
  {AdjustedBits, _} = lists:split(BitsLength - ToDrop, Bits),
  bits_to_binary(AdjustedBits, []).

base32_to_bits([], Bits) ->
  lists:reverse(Bits);
base32_to_bits([B | Base32], Bits) ->
  <<_:3, B4:1, B3:1, B2:1, B1:1, B0:1>> = <<B:8>>,
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
