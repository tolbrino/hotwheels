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

-module(janus_app).
-behaviour(application).

-export([start_transport/1]).
-export([start/2, stop/1, init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(LISTEN_PORT, 8081).

%% A startup function for new client connection handling.
%% To be called by the TCP listener process.

start_transport(Port) ->
    supervisor:start_child(janus_transport_sup, [Port]).

start(_Type, _Args) ->
    Port = janus_admin:get_env(listen_port, ?LISTEN_PORT),
    supervisor:start_link({local, ?MODULE}, 
                          ?MODULE, 
                          [Port, transport]).

stop(_S) ->
    ok.

%% Supervisor behaviour callbacks

init([Port, Module]) ->
    {ok,
     {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% TCP server
       {janus_sup,
        {janus_acceptor, start_link, [self(), Port, Module]},
        permanent,
        2000,
        worker,
        [janus_acceptor]
       },
       %% Topic manager
       {janus_topman_sup,
        {topman, start, []},
        permanent,
        2000,
        worker,
        [topman]
       },
       %% Client proxy mapper
       {janus_proxy_mapper_sup,
        {mapper, start, [client_proxy_mapper]},
        permanent,
        2000,
        worker,
        [mapper]
       },
       %% Client instance supervisor
       {janus_transport_sup,
        {supervisor, start_link, [{local, janus_transport_sup}, 
                                  ?MODULE, [Module]]},
        permanent,
        infinity,
        supervisor,
        []
       }
      ]
     }
    };

init([Module]) ->
    {ok,
     {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
      [
       %% TCP Client
       {undefined,
        {Module, start_link, []},
        temporary,
        2000,
        worker,
        []
       }
      ]
     }
    }.

