-module(bech_tests).
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
    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqzqx55gg7">>}, bech32:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16>>)),
    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqrudk4q8v">>}, bech32:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31>>)),
    ?_assertEqual({ok, <<"split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w">>}, bech32:encode(<<"split">>, <<197, 243, 139, 112, 48, 95, 81, 155, 246, 109, 133, 251, 108, 240, 48, 88, 243, 221, 228, 99, 236, 215, 145, 143, 45, 199, 67, 145, 143, 45>>)),
    ?_assertEqual({ok, <<"ordered1qpzry9x8gf2tvdw0s3jn54khce6mua7lnzq8ey">>}, bech32:encode(<<"ordered">>, <<0,68,50,20,199,66,84,182,53,207,132,101,58,86,215,198, 117,190,119,223>>)),
    ?_assertEqual({ok, <<"ORDERED1QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7LNZQ8EY">>}, bech32:encode(<<"ordered">>, <<0,68,50,20,199,66,84,182,53,207,132,101,58,86,215,198, 117,190,119,223>>, upper))
  ].

encode_invalid_test_() ->
  [
    % no HRP
    ?_assertEqual({error, no_hrp}, bech32:encode(<<"">>, <<>>)),
    % no HRP
    ?_assertEqual({error, no_hrp}, bech32:encode(<<>>, <<>>)),
    % invalid character 0x20 in HRP
    ?_assertEqual({error, invalid_hrp_character}, bech32:encode(<<16#20>>, <<>>)),
    % invalid character 0x7F in HRP
    ?_assertEqual({error, invalid_hrp_character}, bech32:encode(<<16#7F>>, <<>>)),
    % invalid character 0x80 in HRP
    ?_assertEqual({error, invalid_hrp_character}, bech32:encode(<<16#80>>, <<>>)),
    % HRP is too long, maximum allowed length is 83 characters
    ?_assertEqual({error, hrp_length_exceeded, 84}, bech32:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<>>)),
    % total length of the address is exceeded, maximum allowed length is 90 characters
    ?_assertEqual({error, total_length_exceeded, 91}, bech32:encode(<<"abcd">>, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>))
  ].

decode_valid_test_() ->
  [
    ?_assertEqual({ok, <<"a">>, <<>>}, bech32:decode(<<"a12uel5l">>)),
    ?_assertEqual({ok, <<"a">>, <<>>}, bech32:decode(<<"A12UEL5L">>)),
    ?_assertEqual({ok, <<"?">>, <<>>}, bech32:decode(<<"?1ezyfcl">>)),
    ?_assertEqual({ok, <<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio">>, <<>>}, bech32:decode(<<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs">>)),
    ?_assertEqual({ok, <<"abcdef">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>}, bech32:decode(<<"abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw">>)),
    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>}, bech32:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j">>)),
    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16>>}, bech32:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqzqx55gg7">>)),
    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31>>}, bech32:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqrudk4q8v">>)),
    ?_assertEqual({ok, <<"split">>, <<197, 243, 139, 112, 48, 95, 81, 155, 246, 109, 133, 251, 108, 240, 48, 88, 243, 221, 228, 99, 236, 215, 145, 143, 45, 199, 67, 145, 143, 45>>}, bech32:decode(<<"split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w">>))
  ].

decode_invalid_test_() ->
  [
    % invalid character 0x20 in HRP
    ?_assertEqual({error, invalid_hrp_character}, bech32:decode(<<16#20, "1nwldj5">>)),
    % invalid character 0x7F in HRP
    ?_assertEqual({error, invalid_hrp_character}, bech32:decode(<<16#7F, "1axkwrx">>)),
    % invalid character 0x80 in HRP
    ?_assertEqual({error, invalid_hrp_character}, bech32:decode(<<16#80, "1eym55h">>)),
    % total length of the address is exceeded, maximum allowed length is 90 characters
    ?_assertEqual({error, total_length_exceeded, 91}, bech32:decode(<<"an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx">>)),
    % no HRP separator (no letter '1')
    ?_assertEqual({error, no_hrp_separator}, bech32:decode(<<"pzry9x0s0muk">>)),
    % no HRP
    ?_assertEqual({error, no_hrp}, bech32:decode(<<"1pzry9x0s0muk">>)),
    % invalid character in data (letter 'b')
    ?_assertEqual({error, invalid_character, 98}, bech32:decode(<<"x1b4n0q5v">>)),
    % too short checksum
    ?_assertEqual({error, too_short_checksum}, bech32:decode(<<"li1dgmt3">>)),
    % invalid character in checksum (letter 'b')
    ?_assertEqual({error, invalid_character, 255}, bech32:decode(<<"de1lg7wt", 16#FF>>)),
    % checksum calculated with uppercase form of HRP, so is invalid
    ?_assertEqual({error, invalid_checksum}, bech32:decode(<<"A1G7SGD8">>)),
    % no HRP and no data
    ?_assertEqual({error, no_hrp}, bech32:decode(<<"10a06t8">>)),
    % empty HRP with some data
    ?_assertEqual({error, no_hrp}, bech32:decode(<<"1qzzfhee">>))
  ].
