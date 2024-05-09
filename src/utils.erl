-module(utils).
-export([get_option/2, get_option/3]).

%%%=============================================================================
%%%  Public functions
%%%=============================================================================

% Returns an option with specified key (as a singleton list)
% or an empty list when specified key was not found in options.
%
-spec get_option(Key :: atom(), Options :: list()) -> list().
get_option(Key, Options) when is_atom(Key), is_list(Options) ->
  case [OptionValue || {OptionKey, OptionValue} <- lists:reverse(Options), OptionKey == Key] of
    [] -> [];
    [Value] -> [Value];
    [Value | _] -> [Value]
  end.

% Returns an option with specified key (as a singleton list)
% or the default value when specified key was not found in options.
%
-spec get_option(Key :: atom(), Options :: list(), Default :: any()) -> list().
get_option(Key, Options, Default) when is_atom(Key), is_list(Options) ->
  case get_option(Key, Options) of
    [] -> [Default];
    Other -> Other
  end.

%%%============================================================================
%%% Unit tests
%%%============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

get_option_default_test_() ->
  [
    % when no options specified, returns the default value
    ?_assertEqual([10], get_option(key, [], 10)),
    % when option key is found, returns the value
    ?_assertEqual([1], get_option(key, [{key, 1}], 10)),
    % when multiple options specified and the key is found, returns the value
    ?_assertEqual([1], get_option(key1, [{key1, 1}, {key2, 2}], 10)),
    % when multiple options specified and the key is found, returns the value
    ?_assertEqual([2], get_option(key2, [{key1, 1}, {key2, 2}], 10)),
    % when multiple options specified and no key is found, returns th default value
    ?_assertEqual([10], get_option(key3, [{key1, 1}, {key2, 2}], 10)),
    % when multiple options specified with the same key, returns the last value
    ?_assertEqual([2], get_option(key1, [{key1, 1}, {key1, 2}, {key2, 3}, {key2, 4}, {key2, 5}], 10)),
    % when multiple options specified with the same key, returns the last value
    ?_assertEqual([5], get_option(key2, [{key1, 1}, {key1, 2}, {key2, 3}, {key2, 4}, {key2, 5}], 10))
  ].

-endif.
