-module(triton_tests).

-include_lib("eunit/include/eunit.hrl").

-test(active_t).

-export([active_t/0]).

active_t() ->
    9 = 10 - 1.

a_test() ->
    ?assertMatch(a,a).


b_test() ->
    ?assertMatch(a,b).

    