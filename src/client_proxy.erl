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

-module(client_proxy).
-behavior(gen_server).

-export([start/1, stop/1, locate/1, poll/1,
         attach/2, detach/1]).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%%% Timeout between requests to pick up messages (poll)

-define(HEARTBEAT, 30000).

-record(state, {
          token,
          parent,
          send,
          heartbeat,
          killswitch,
          messages
         }).

locate(Token) ->
    mapper:where(client_proxy_mapper, Token).

poll(Ref) ->
    gen_server:call(Ref, messages).

attach(Ref, Send) ->
    gen_server:cast(Ref, {attach, self(), Send}).

detach(Ref) ->
    gen_server:cast(Ref, {detach, self()}).

start(Send) ->
    Token = common:random_token(),
    {ok, Pid} = gen_server:start_link(?MODULE, [Token, self(), Send], []),
    {ok, Pid, Token}.

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([Token, Parent, Send]) ->
    process_flag(trap_exit, true),
    ok = mapper:add(client_proxy_mapper, Token),
    State = #state{
      token = Token,
      parent = Parent,
      send = Send,
      messages = []
     },
   {ok, State}.

handle_cast({attach, Parent, Send}, State) ->
    {noreply, State#state{parent = Parent, send = Send}};

handle_cast({detach, Who}, State) 
  when Who == State#state.parent ->
    %% transport is gone, session stays
    Send = fun(_) -> ok end,
    {noreply, State#state{parent = disconnected, send = Send}};

handle_cast({detach, _}, State) ->
    {noreply, State};

handle_cast({<<"subscribe">>, Topic}, State) ->
    topman:subscribe(self(), Topic, State#state.send),
    {noreply, State};

handle_cast({<<"unsubscribe">>, Topic}, State) ->
    topman:unsubscribe(self(), Topic),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call(messages, _From, State) ->
    State1 = start_heartbeat(State),
    {reply, State1#state.messages, State1#state{messages = []}};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info(Event = {message, Msg}, State) 
  when is_pid(State#state.parent),
       is_binary(Msg) ->
    %% send immediately
    %% gen_server:cast(State#state.parent, Event),
    State#state.parent ! Event,
    {noreply, start_heartbeat(State)};

handle_info({message, Msg}, State) ->
    %% buffer messages
    Messages1 = [Msg|State#state.messages],
    {noreply, State#state{messages = Messages1}};

handle_info(heartbeat, State) 
  when is_pid(State#state.parent) ->
    State#state.parent ! heartbeat,
    {noreply, State};

handle_info(heartbeat, State) ->
    %% no transport attached
    {noreply, State};
    
handle_info(ack, State) 
  when is_pid(State#state.parent) ->
    State#state.parent ! ack,
    {noreply, State};

handle_info(ack, State) ->
    %% no transport attached
    Messages1 = [<<"'ack'">>|State#state.messages],
    {noreply, State#state{messages = Messages1}};
    
handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(Event, State) ->
    {stop, {unknown_info, Event}, State}.

terminate(_Reason, State) ->
    cancel_heartbeat(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cancel_heartbeat(State) ->
    catch erlang:cancel_timer(State#state.heartbeat),
    State#state{heartbeat = undefined}.
    
start_heartbeat(State) ->
    Timer = erlang:send_after(?HEARTBEAT, self(), heartbeat),
    State1 = cancel_heartbeat(State),
    State1#state{heartbeat = Timer}.

