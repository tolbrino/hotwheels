%%% Copyright (c) 2009 Oortle, Inc

%%% Permission is hereby granted, free of charge, to any person 
%%% obtaining a copy of this software and associated documentation 
%%% files (the "Software"), to deal in the Software without restriction, 
%%% including without limitation the rights to use, copy, modify, merge, 
%%% publish, distribute, sublicense, and/or sell copies of the Software, 
%%% and to permit persons to whom the Software is furnished to do so, 
%%% subject to the following conditions:

%%% The above copyright notice and this permission notice shall be included 
%%% in all copies or substantial portions of the Software.

%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
%%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
%%% DEALINGS IN THE SOFTWARE.

-module(t).

-compile([export_all]).

-include_lib("stdlib/include/ms_transform.hrl").

ut() ->
    dbg:ctp().

t(Mod) 
  when is_atom(Mod) ->
    t([Mod]);

t({Mod, Fun}) 
  when is_atom(Mod),
       is_atom(Fun) ->
    t([{Mod, Fun}]);
  
t(What) ->
    dbg:tracer(),
    dbg:p(all, [call]),
    t1(What).

t1([]) ->
    ok;

t1([{M, F}|T]) ->
    dbg:tpl(M, F, dbg:fun2ms(fun(_) -> return_trace() end)),
    t1(T);

t1([H|T]) ->
    dbg:tpl(H, dbg:fun2ms(fun(_) -> return_trace() end)),
    t1(T).

mods2procs(Mods) ->
    [P || M <- Mods, P <- processes(), erlang:check_process_code(P, M)].

p(Mod) ->
    p(Mod, processes(), []).

p(_, [], Acc) ->
    Acc;

p(Mod, [H|T], Acc) ->
    {_, L} = process_info(H, dictionary),
    case lists:keyfind('$initial_call', 1, L) of
        {'$initial_call', {Mod, init, 1}} ->
            p(Mod, T, [H|Acc]);
        _ ->
            p(Mod, T, Acc)
    end.

