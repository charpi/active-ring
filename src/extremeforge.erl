%%% Copyright (C) Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (extremeforge).
-export ([main/1]).
-export ([start/1]).
-export ([start/0]).
-export ([run/1]).
-export ([run/0]).
-export ([stop/0]).

main (Params) ->
    Config = read_config (Params, []),
    Mode = proplists: get_value (mode, Config, start),
    Directories = proplists: get_value (directories, Config, []),
    Output = proplists: get_value (output, Config, console),
    launch (Mode, Directories, start_output (Output)).

start () ->
    start ([]).

start (Roots) ->
    Printer = spawn_link (text_printer, init, [standard_io]),
    register (extremeforge, spawn_link (fun () -> launch(start, Roots, Printer) end)).

run () ->
    run ([]).

run (Roots) ->
    Printer = spawn_link (text_printer, init, [standard_io]),
    launch(run, Roots, Printer).

stop () ->
    extremeforge ! stop,
    unregister (extremeforge).

launch(Mode, [], Printer) ->
    {ok, CWD} = file: get_cwd (),
    launch (Mode, [CWD], Printer);
launch(start, Arguments, Printer) ->
    foobar (Arguments, fun checker_loop/3, Printer, Printer);
launch(run, Arguments, Printer) ->
    case foobar (Arguments, fun one_time_loop/3, self(), Printer) of
	ok ->
	    init: stop (0);
	fails ->
	    init: stop (1)
    end.

foobar (Roots, Loop, Mux, State) ->
    Rs = [filename: absname (R) || R <- Roots],
    Integrator = spawn_link (integrator, init, [Mux, Rs, []]),
    F = fun (E) -> Integrator ! E end,
    Ws = [spawn_link (directory_watcher, init_recursive, [R, F]) || R <- Rs],
    [Pid ! check || Pid <- Ws],
    Loop (Ws,Integrator, State).
    
checker_loop(Watchers, Integrator, State) ->
    receive stop ->
	    [Pid ! stop || Pid <- [State, Integrator| Watchers]],
	    ok
    after 1000 ->
	    [Pid ! check || Pid <- Watchers],
	    checker_loop (Watchers, Integrator, State)
    end.

one_time_loop(Watchers, Integrator, State) ->
    receive Msg -> 
	    State ! Msg,
	    case Msg of
		{totals, {M, C, E, _, _, _}} when M > C + E ->
		    one_time_loop (Watchers, Integrator, State);
		{totals, {M, M, 0, T, P, F}} when T > P + F ->
		    one_time_loop (Watchers, Integrator, State);
		{totals, {M, C, E, _, _, _}=Totals} when M == C + E ->
		    stop (Totals, [State,Integrator|Watchers]);
		_ ->
		    one_time_loop (Watchers, Integrator, State)
	    end
    end.

stop (Totals, Processes) ->
    [Pid ! stop || Pid <- Processes],
    exit_code (Totals).

exit_code ({M, M, 0, T, T, 0}) ->
    ok;
exit_code (_) ->
    fails.

read_config ([], Acc) ->
    Acc;
read_config (["-snapshot"| Rest], Acc) ->
    read_config (Rest, [{mode, run}|Acc]);
read_config (["-output", Module| Rest], Acc) ->
    read_config (Rest, [{output, list_to_atom(Module)}|Acc]);
read_config ([Other|Rest], Acc) ->
    Old = proplists:get_value (directories, Acc,[]),
    New = {directories, [Other] ++ Old},
    read_config (Rest, [ New | proplists: delete (directories, Acc)]).

start_output (console) ->
    spawn_link (text_printer, init, [standard_io]);
start_output (Module) ->
    spawn_link (Module, init, []).
