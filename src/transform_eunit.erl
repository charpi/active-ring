%%% Copyright (C) Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.
-module(transform_eunit).

-export([parse_transform/2]).

parse_transform (Forms, _Options) ->
    transform_eunit_tests(is_eunit(Forms), Forms).

is_eunit([]) ->
    false;
is_eunit([{attribute,_,file,{Name,_}}|Tail]) ->
    case is_eunit_header(Name) of
	true ->
	    true;
	false ->
	    is_eunit(Tail)
    end;
is_eunit([_|Tail]) ->
    is_eunit(Tail).

is_eunit_header(Name) ->
    0 =/= string:str(Name,"eunit.hrl").

transform_eunit_tests(false, Forms) ->
    Forms;
transform_eunit_tests(true, Forms) ->
    {Before,After} = split_attributes(Forms),
    Before ++ tests(Forms) ++ After.


split_attributes(Forms) ->
    split_attributes(lists:reverse(Forms),[]).

split_attributes([{attribute,_,_,_} = Att|Other], Acc) ->
    {lists:reverse([Att|Other]), Acc};
split_attributes([X|Otther],Acc) ->
    split_attributes(Otther,[X|Acc]).


tests(Forms) ->
    tests(Forms, []).

tests([], Acc) ->
    Acc;
tests([{function,_,Name,0,_}|Tail], Acc) ->
    NewAcc = case '?end_with_test'(atom_to_list(Name)) of
		 true ->
		     [{attribute,1,test,Name}|Acc];
		 false ->
		     Acc
	     end,
    tests(Tail,NewAcc);
tests([_|Tail], Acc) ->
    tests(Tail,Acc).

'?end_with_test'(String) ->
    0 =/= string:str(String,"_test").
