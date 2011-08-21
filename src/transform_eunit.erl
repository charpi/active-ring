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
    {Attributes, Code} = tests(Forms),
    Before ++ Attributes ++ Code ++ After.


split_attributes(Forms) ->
    split_attributes(lists:reverse(Forms),[]).

split_attributes([{attribute,_,_,_} = Att|Other], Acc) ->
    {lists:reverse([Att|Other]), Acc};
split_attributes([X|Otther],Acc) ->
    split_attributes(Otther,[X|Acc]).


tests(Forms) ->
    tests(Forms, {[],[]}).

tests([], Acc) ->
    Acc;
tests([{function,_,Name,0,Body}|Tail], {Attr,Code}) ->
    NewAcc = case '?test'(atom_to_list(Name)) of
		 simple ->
		     {[export(Name)|Attr], Code};
		 generator ->
		     Res = generate_generator(Name, Body, {Attr,Code}),
		     io:format("~p~n",[Res]),
		     Res;
		 false ->
		     {Attr,Code}
	     end,
    tests(Tail,NewAcc);
tests([_|Tail], Acc) ->
    tests(Tail,Acc).

'?test'(String) ->
    case string:str(String,"_test_") of
	0 ->
	    case string:str(String, "_test") of
		X when X+4 == length(String) ->
		    simple;
		_ ->
		    false
	    end;
	X when X+5 == length(String) ->
	    generator;
	_ ->
	    false
    end.

export(Name) ->
    {attribute,1,test,Name}.

generate_generator(Name, [{clause,_,[],[],[{'fun',_,_}]}]=Clause, {Attr,Code}) ->
    ActiveRingName = list_to_atom(atom_to_list(Name) ++ "_activering"),
    [{clause,Line,[],[],[Fun]}] = Clause,
    Wrapper = {function,Line,ActiveRingName,0,
	       [{clause,Line,[],[],[{call,Line,{atom,Line,apply},
				    [Fun, {nil, Line}]}]}]},
    {[export(ActiveRingName),
      {attribute,0,export,[{ActiveRingName,0}]}
      |Attr],[Wrapper|Code]};
generate_generator(Name, [{clause,Line,[],[],[{tuple,_,Elts}]}], {Attr,Code}) 
  when length(Elts) == 2 ->
    case lists:all(fun({atom,_,_}) -> true ; (_) -> false end, Elts) of
	false -> {Attr,Code};
	true ->
	    ActiveRingName = list_to_atom(atom_to_list(Name) ++ "_activering"),
	    Wrapper = {function,Line,ActiveRingName,0,
		       [{clause,Line,[],[],[{call,Line,{atom,Line,apply},
					     Elts ++[{nil, Line}]}]}]},
	    
	    {[export(ActiveRingName), 
	      {attribute,0,export,[{ActiveRingName,0}]} | Attr],
	     [Wrapper|Code]}
	      
    end.
