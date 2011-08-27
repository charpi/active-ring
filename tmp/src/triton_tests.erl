-module(triton_tests).

-include_lib("eunit/include/eunit.hrl").

-test(['4_active_t']).
-export(['4_active_t'/0]).
-export([dumb_fun/0]).

'1_test'() ->
    ?assertMatch(a,a).

'2_test'() ->
    ?assertMatch(a,a).

'3_basic_test_'() ->    
     fun() ->
 	    ?assertMatch(a,a)
     end.

'4_active_t'() ->
    9 = 10 - 1.

'5_basic_test_'() ->
    fun dumb_fun/0.

'6_basic_test_'() ->
    {?MODULE, dumb_fun}.

'7_basic_test_'() ->
    {1, fun () -> ?assertMatch(a,a) end}.

'8_basic_test_'() ->
    {1, fun dumb_fun/0}.

'9_basic_test_'() ->
    {1, {?MODULE, dumb_fun}}.

'10_test_'() ->
    Value = a,
    fun() ->
	    ?assertMatch(a, Value)
    end.



dumb_fun() ->
    9 = 10 -1.
