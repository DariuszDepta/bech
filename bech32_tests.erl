-module(bech32_tests).
-include_lib("eunit/include/eunit.hrl").

encode_valid_test_() ->
  [
    ?_assertEqual({ok, <<"a12uel5l">>}, bech32:encode(<<"a">>, <<>>)),
    ?_assertEqual({ok, <<"a12uel5l">>}, bech32:encode(<<"a">>, <<>>, lower)),
    ?_assertEqual({ok, <<"A12UEL5L">>}, bech32:encode(<<"a">>, <<>>, upper)),
    ?_assertEqual({ok, <<"?1ezyfcl">>}, bech32:encode(<<"?">>, <<>>)),
    ?_assertEqual({ok, <<"!1wctc0x">>}, bech32:encode(<<33>>, <<>>)),
    ?_assertEqual({ok, <<"~1qszm75">>}, bech32:encode(<<126>>, <<>>)),
    ?_assertEqual({ok, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa17vhfd0">>}, bech32:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<>>)),
    ?_assertEqual({ok, <<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs">>}, bech32:encode(<<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio">>, <<>>)),
    ?_assertEqual({ok, <<"abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw">>}, bech32:encode(<<"abcdef">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>)),
    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j">>}, bech32:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>)),
    ?_assertEqual({ok, <<"split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w">>}, bech32:encode(<<"split">>, <<197, 243, 139, 112, 48, 95, 81, 155, 246, 109, 133, 251, 108, 240, 48, 88, 243, 221, 228, 99, 236, 215, 145, 143, 45, 199, 67, 145, 143, 45>>))
  ].

encode_invalid_test_() ->
  [
    ?_assertEqual({error, no_hrp}, bech32:encode(<<"">>, <<>>)),
    ?_assertEqual({error, no_hrp}, bech32:encode(<<>>, <<>>)),
    ?_assertEqual({error, invalid_hrp_character}, bech32:encode(<<32>>, <<>>)),
    ?_assertEqual({error, invalid_hrp_character}, bech32:encode(<<127>>, <<>>)),
    ?_assertEqual({error, too_long_hrp}, bech32:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<>>)),
    ?_assertEqual({error, too_long}, bech32:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>))
  ].

decode_valid_test_() ->
  [
    ?_assertEqual({ok, <<"a">>, <<>>}, bech32:decode(<<"a12uel5l">>)),
    ?_assertEqual({ok, <<"a">>, <<>>}, bech32:decode(<<"A12UEL5L">>)),
    ?_assertEqual({ok, <<"?">>, <<>>}, bech32:decode(<<"?1ezyfcl">>))
  ].
