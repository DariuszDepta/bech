-module(utils).
-export([get_option/2]).

-include_lib("eunit/include/eunit.hrl").

get_option(Key, Options) when is_atom(Key), is_list(Options) ->
  case [OptionValue || {OptionKey, OptionValue} <- lists:reverse(Options), OptionKey == Key] of
    [] -> [];
    [Value] -> [Value];
    [Value | _] -> [Value]
  end.

%%% ============================================================================
%%%  Unit tests
%%% ============================================================================

get_option_test_() ->
  [
    % when no options specified, returns an empty list of values
    ?_assertEqual([], get_option(key, [])),
    % when option key is found, returns the value
    ?_assertEqual([1], get_option(key, [{key, 1}])),
    % when multiple options specified and the key is found, returns the value
    ?_assertEqual([1], get_option(key1, [{key1, 1}, {key2, 2}])),
    % when multiple options specified and the key is found, returns the value
    ?_assertEqual([2], get_option(key2, [{key1, 1}, {key2, 2}])),
    % when multiple options specified and no key is found, returns an empty list
    ?_assertEqual([], get_option(key3, [{key1, 1}, {key2, 2}])),
    % when multiple options specified with the same key, returns the last value
    ?_assertEqual([2], get_option(key1, [{key1, 1}, {key1, 2}, {key2, 3}, {key2, 4}, {key2, 5}])),
    % when multiple options specified with the same key, returns the last value
    ?_assertEqual([5], get_option(key2, [{key1, 1}, {key1, 2}, {key2, 3}, {key2, 4}, {key2, 5}]))
  ].
