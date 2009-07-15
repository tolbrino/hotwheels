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

-module(barrier).
-behaviour(gen_server).

-export([start/2, stop/1, bump/1, wait/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-record(barrier, {
          target,
          counter,
          timer
         }).

start(time, Target) 
  when is_tuple(Target) ->
    gen_server:start(barrier, [time, Target], []);

start(counter, Target) 
  when is_integer(Target) ->
    gen_server:start(barrier, [counter, Target], []).

init([time, Time]) 
  when is_tuple(Time) ->
    process_flag(trap_exit, true),
    Delta = case Time of
                {seconds, N} ->
                    timer:seconds(N);
                {minutes, N} ->
                    timer:minutes(N)
            end,
    {ok, Timer} = timer:apply_after(Delta, barrier, stop, [self()]), 
    {ok, #barrier{timer = Timer}};

init([counter, Target]) 
  when is_integer(Target) ->
    process_flag(trap_exit, true),
    {ok, #barrier{target = Target, counter = 0}}.

stop(Barrier)
  when is_pid(Barrier) ->
    gen_server:cast(Barrier, {stop, self()}).

bump(Barrier) ->
    gen_server:cast(Barrier, 'BUMP').

terminate(_Reason, State) 
  when State#barrier.timer /= undefined ->
    catch timer:cancel(State#barrier.timer),
    ok;

terminate(_Reason, _State) ->
    ok.

handle_cast('BUMP', State) 
  when State#barrier.counter + 1 >= State#barrier.target ->
    {stop, normal, State};

handle_cast('BUMP', State) ->
    N = State#barrier.counter,
    {noreply, State#barrier{ counter = N + 1}};

handle_cast({'TARGET', N}, State) ->
    {noreply, State#barrier{ target = N }};

handle_cast({stop, _Pid}, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call('COUNTER', _From, State) ->
    {reply, State#barrier.counter, State};

handle_call('TARGET', _From, State) ->
    {reply, State#barrier.target, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% child exit?
    {noreply, State};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

wait(Barrier) ->
    TE = process_flag(trap_exit, true),
    link(Barrier),
    receive {'EXIT', Barrier, normal} -> ok end,
    process_flag(trap_exit, TE),
    ok.

%%%
%%% Test suite
%%% 

counter_test() ->
    {ok, B} = start(counter, 2),
    ?assertEqual(true, util:is_process_alive(B)),
    bump(B),
    ?assertEqual(true, util:is_process_alive(B)),
    bump(B),
    timer:sleep(100),
    ?assertEqual(false, util:is_process_alive(B)),
    ok.

timer_test() ->    
    {ok, B} = start(time, {seconds, 2}),
    ?assertEqual(true, util:is_process_alive(B)),
    timer:sleep(2000),
    ?assertEqual(false, util:is_process_alive(B)),
    ok.

wait_test() ->
    {ok, B} = start(time, {seconds, 2}),
    ?assertEqual(ok, wait(B)),
    ok.

