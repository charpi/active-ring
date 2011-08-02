%%% Copyright (C) Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (packer).

-export ([pack/0]).


pack () ->
    Files = filelib: wildcard ("*.beam","ebin"),
    {ok, {_, Beam}} = zip:create ("dummy", Files, [memory, {cwd, "ebin"}]),
    Arguments = "-sname activering -escript main extremeforge",
    Options = [{archive, Beam}, shebang, {emu_args, Arguments}],
    ok = escript: create ("activering", Options),
    ok.
    
