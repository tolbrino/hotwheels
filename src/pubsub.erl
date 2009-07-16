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

-module(pubsub).

-export([publish/2, subscribe/2, unsubscribe/2,
         start/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          topic,
          subs = []
         }).

publish(Ref, Msg) ->
    gen_server:cast(Ref, {publish, Msg}).

subscribe(Ref, Pid) ->
    gen_server:cast(Ref, {subscribe, Pid}).

unsubscribe(Ref, Pid) ->
    gen_server:cast(Ref, {unsubscribe, Pid}).

start(Topic) ->
    gen_server:start_link(?MODULE, [Topic], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([Topic]) ->
    process_flag(trap_exit, true),
    {ok, #state{topic = Topic}}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({subscribe, Pid}, State) ->
    %% automatically unsubscribe when dead
    Ref = erlang:monitor(process, Pid),
    L = lists:keydelete(Pid, 1, State#state.subs),
    Pid ! ack,
    {noreply, State#state{subs = [{Pid, Ref}|L]}};

handle_cast({unsubscribe, Pid}, State) ->
    unsubscribe1(Pid, State);

handle_cast({publish, Msg}, State) ->
    io:format("info: ~p~n", [length(State#state.subs)]),
    Start = now(),
    {struct, L} = Msg,
    TS = binary_to_list(term_to_binary(Start)),
    JSON = {struct, [{<<"timestamp">>, TS}|L]},
    Msg1 = {message, iolist_to_binary(mochijson2:encode(JSON))},
    [ P ! Msg1 || {P, _} <- State#state.subs ],
    io:format("time: ~p~n", [timer:now_diff(now(), Start) / 1000]),
    {noreply, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    unsubscribe1(Pid, State);

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

unsubscribe1(Pid, State) ->
    case lists:keyfind(Pid, 1, State#state.subs) of
        false ->
            ok;
        {Pid, Ref} ->
            erlang:demonitor(Ref)
    end,
    L = lists:keydelete(Pid, 1, State#state.subs),
    {noreply, State#state{subs = L}}.

