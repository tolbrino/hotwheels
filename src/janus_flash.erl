-module(janus_flash).

-export([process/2, forward/2, start/1, stop/1]).

-record(state, {
          socket,
          data,
          proxy,
          token
         }).

start(Socket) ->
    {ok, Proxy, Token} = client_proxy:start(),
    State = #state{
      socket = Socket, 
      proxy = Proxy, 
      token = Token
     },
    JSON = {struct,
            [{<<"timestamp">>, binary_to_list(term_to_binary(now()))},
             {<<"token">>, Token}
            ]},
    Bin = mochijson2:encode(JSON),
    ok = gen_tcp:send(State#state.socket, [Bin, 1]),
    {ok, State}.

stop(State) ->
    catch client_proxy:detach(State#state.proxy),
    ok.

forward(Bin, State)
  when is_binary(Bin) ->
    ok = gen_tcp:send(State#state.socket, [Bin, 1]),
    {ok, State}.

process(heartbeat, State) ->
    ok = gen_tcp:send(State#state.socket, <<"PING", 1>>),
    {ok, keep_alive, State};

process(ack, State) ->
    ok = gen_tcp:send(State#state.socket, <<"ACK", 1>>),
    {ok, keep_alive, State};

process(<<>>, State) ->
    {ok, keep_alive, State};

process(Bin, State) 
  when is_binary(State#state.data),
       is_binary(Bin) ->
    process(list_to_binary([State#state.data, Bin]),
            State#state{data = undefined});

process(<<"<regular-socket/>", 0, Bin/binary>>, State) ->
    process(Bin, State);

process(Bin, State) 
  when is_binary(Bin) ->
    process(bin:split("\\000", Bin), State);

process({more, Bin}, State) ->
    {ok, keep_alive, State#state{data = Bin}};

process({ok, <<>>, <<>>}, State) ->
    {ok, keep_alive, State};

process({ok, <<>>, Rest}, State) ->
    process(Rest, State);

process({ok, <<"PING">>, Rest}, State) ->
    process(Rest, State);

process({ok, <<"PONG">>, Rest}, State) ->
    process(Rest, State);

process({ok, <<"PUBLISH">>, Rest}, State) ->
    JSON = {struct, [{<<"topic">>, Topic},
                     {<<"event">>, _},
                     {<<"message_id">>, _},
                     {<<"data">>, _}
                    ]} = mochijson2:decode(Rest),
    topman:publish(JSON, Topic),
    {ok, shutdown, State};

process({ok, Bin, Rest}, State) ->
    {struct,
     [{<<"action">>, Action}, 
      {<<"data">>, Topic}
     ]} = mochijson2:decode(Bin),
    gen_server:cast(State#state.proxy, {Action, Topic}),
    process(Rest, State).

