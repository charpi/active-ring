%%% Copyright (c) Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (modules).
-export ([to_binary/1]).
-export ([forms_to_binary/1]).
-export ([module_name/1]).
-export ([locate/2]).
-export ([includes/1]).
-export ([compile/2]).
-export (['OTP_include_dir'/1]).
-export ([member_of_includes/3]).
-export ([normalise_filename/1]).

compile (File_name, Options) ->
    case compile (fun compile: file/2, File_name, Options) of
	{ok, Module, Binary, Warnings} ->
	    {File_name, Module, Tests} = tests: filter_by_attribute (Binary),
	    {File_name, Module, ok, {Binary, Tests, Warnings}};
	{error, Errors, Warnings} ->
	    Module = module_name (File_name),
	    {File_name, Module, error, {Errors, Warnings}}
    end.

compile (Fun, Parameter, User_options) ->
    Options = [binary, return, warn_unused_import, debug_info | User_options],
    Fun (Parameter, Options).

to_binary (File_name) ->
    {ok, _, Binary, _} = compile (fun compile: file/2, File_name, []),
    Binary.

forms_to_binary (Forms) ->
    {ok, _, Binary, _} = compile (fun compile: forms/2, Forms, []),
    Binary.

module_name (Filename) ->
    {extension, ".erl"} = {extension, filename: extension (Filename)},
    String = filename: rootname (filename: basename (Filename)),
    list_to_atom (String).

locate ({M, F, A}, Binary) ->
    {ok, {M, Chunks}} = beam_lib: chunks (Binary, [abstract_code, compile_info]),
    [{abstract_code, Code}, {compile_info, Info}] = Chunks,
    {value, {source, Filename}} = lists: keysearch (source, 1, Info),
    {raw_abstract_v1, Forms} = Code,
    Line = locate_line (F, A, Forms),
    {Filename, Line}.

locate_line (Function, Arity, [{function, Line, Function, Arity,  _} | _]) ->
    Line;
locate_line (Function, Arity, [_ | Forms]) ->
    locate_line (Function, Arity, Forms);
locate_line (_, _, []) ->
    unknown.

includes (File) ->
    includes_from_forms (epp_dodger: parse_file (File)).

includes_from_forms ({error, _}=E) ->
    E;
includes_from_forms ({ok, Forms}) ->
    Attributes = [A || {tree, attribute, _, A} <- Forms],
    Includes = [I || {attribute, {atom, _, include}, [I]} <- Attributes],
    [F || {string, _, F} <- Includes].

'OTP_include_dir' (File) ->
    Local = filename: dirname (File),
    Top = filename: dirname (Local),
    filename: join (Top, "include").

member_of_includes (File, Source, Includes) ->
    lists: any (member_of_include_fun (File, Source), Includes).

member_of_include_fun (File, Source) ->
    fun (Include) ->
	    Potential = potential_includes (Include, Source),
	    lists: member (File, Potential)
    end.

potential_includes (Include, Source) ->
    Source_dir = filename: dirname (Source),
    Absolute_include = filename: join (Source_dir, Include),
    OTP_include_dir = 'OTP_include_dir' (Source),
    Absolute_OTP_include = filename: join (OTP_include_dir, Include),
    Absolute_include2 = normalise_filename(Absolute_include),
    [Absolute_include, Absolute_OTP_include, Absolute_include2].

normalise_filename (File) ->
    Dirs = filename: split (File),
    filename: join (normalise_filename (Dirs, [])).

normalise_filename ([".." | Tail], [_ | Acc])->
    normalise_filename (Tail, Acc);
normalise_filename ([Dir | Tail], Acc) ->
    normalise_filename (Tail, [Dir | Acc]);
normalise_filename ([], Acc) ->
    lists: reverse (Acc).

