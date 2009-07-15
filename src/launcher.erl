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

%%% Distributed test launcher

-module(launcher).
-behaviour(gen_server).

-export([start/1, stop/1, launch/2, launch/3, next/0, next/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          trace = false
         }).

new(Trace) ->
    #state{
     trace = Trace
    }.

start(Trace) ->
    gen_server:start_link(?MODULE, [Trace], []).

launch(Mod, Args) ->
    {ok, Pid} = util:get_random_pid(?MODULE),
    launch(Pid, Mod, Args).

launch(Pid, Mod, Args) ->
    gen_server:call(Pid, {launch, Mod, Args}).

next() ->
    next([]).

next(undefined) ->
    next([]);

next([]) ->
    case pg2:get_members(?MODULE) of
        [] ->
            timer:sleep(100),
            next([]);
        L ->
            next(L)
    end;

next([H|T]) ->
    {H, T}.

init([Trace]) ->
    process_flag(trap_exit, true),
    pg2:create(?MODULE),
    ok = pg2:join(?MODULE, self()),
    {ok, new(Trace)}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

terminate(_Reason, _State) ->
    ok.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call({launch, Mod, Args}, _From, State) ->
    Reply = Mod:start(Args),
    {reply, Reply, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'DOWN', _, process, _, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', _, _}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

