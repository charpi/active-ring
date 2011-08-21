-module(triton_tests).

-include_lib("eunit/include/eunit.hrl").

-export([active_t/0]).

a_test() ->
    ?assertMatch(a,a).

b_test() ->
    ?assertMatch(a,b).

basic_test_() ->    
     fun() ->
 	    ?assertMatch(a,c)
     end.

active_t() ->
    9 = 10 - 1.

basic2_test_() ->
    fun active_t/0.

basic3_test_() ->
    {?MODULE, active_t}.
