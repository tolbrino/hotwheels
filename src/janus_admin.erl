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

-module(janus_admin).

-export([make_boot/0,
         get_env/1, get_env/2,
         cluster/0, join/1]).

make_boot() ->
    systools:make_script("janus", [local, {outdir, "./ebin"}]).

%%% Read application configuration variables

get_env(Var) ->
    get_env(Var, undefined).

get_env(Var, Default) ->
    case application:get_env(Var) of
        {ok, Val} -> Val;
        _ -> case init:get_argument(Var) of
                 {ok, [[Val]]} -> Val;
                 {ok, [Val]}   -> Val;
                 _   -> Default
            end
    end.

%%% Nodes to ping to join the cluster

cluster() ->
    Existing = case application:get_env(janus, cluster) of
                   undefined ->
                       [];
                   {ok, L = [_|_]} ->
                       L;
                   {ok, Name} ->
                       [Name]
               end,
    cluster(Existing, []).

cluster([], Acc) ->
    Acc;

cluster([H|T], Acc) ->
    H1 = if
             is_atom(H) ->
                 atom_to_binary(H, latin1);
             true ->
                 H
         end,
    Node = case re:run(H1, <<"@">>) of
               nomatch ->
                   node_name(H1);
               _ ->
                   H
           end,
    cluster(T, [Node|Acc]).
    

%%% Ping and return unreachable nodes

join(Nodes) ->
    F = fun(Node) ->
                pong == net_adm:ping(Node)
        end,
    lists:filter(F, Nodes).

node_name(Host) 
  when is_binary(Host) ->
    binary_to_atom(list_to_binary(["janus@", Host]), latin1).
    
