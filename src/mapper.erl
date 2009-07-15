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

%%%
%%% Lightweight process registry
%%% 

-module(mapper).

-export([add/2, delete/2, where/2, info/1, start/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          key_pid = ets:new(mapper, [set]),
          pid_key = ets:new(mapper, [set])
         }).

add(Ref, Key) ->
    gen_server:call(Ref, {add, Key, self()}).

delete(Ref, Key) ->
    gen_server:cast(Ref, {delete, Key}).

where(Ref, Key) ->
    gen_server:call(Ref, {where, Key}).

info(Ref) ->
    gen_server:call(Ref, info).

start(Name) 
  when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_cast({delete, Pid}, State) ->
    {noreply, do_delete(Pid, State)};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call(info, _, State) ->
    Info = [ets:info(State#state.key_pid),
            ets:info(State#state.pid_key)],
    {reply, Info, State};

handle_call({where, Key}, _, State) ->
    Result = case ets:lookup(State#state.key_pid, Key) of
                 [{_, {Pid, _}}] ->
                     {ok, Pid};
                 _ ->
                     undefined
             end,
    {reply, Result, State};

handle_call({add, Key, Pid}, _, State) ->
    case ets:lookup(State#state.key_pid, Key) of
        [_] ->
            ok;
        _ ->
            Ref = erlang:monitor(process, Pid),
            ets:insert(State#state.key_pid, {Key, {Pid, Ref}}),
            ets:insert(State#state.pid_key, {Pid, Key})
    end,
    {reply, ok, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    {noreply, do_delete(Pid, State)};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_delete(Pid, State) ->
    case ets:lookup(State#state.pid_key, Pid) of
        [{_, Key}] ->
            [{_, {_, Ref}}] = ets:lookup(State#state.key_pid, Key),
            erlang:demonitor(Ref),
            ets:delete(State#state.key_pid, Key),
            ets:delete(State#state.pid_key, Pid);
        _ ->
            ok
    end,
    State.

    


