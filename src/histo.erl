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

-module(histo).

-export([build/2]).

build(Max, Values) ->
    Values1 = lists:sort(Values),
    Edge = [500000, 1000000, 1500000, 2000000, 2500000, 
            3000000, 3500000, 4000000, 4500000, 5000000,
            Max],
    build(Values1, Edge, 0, []).

build([], [Edge|_], Count, Histo) ->
    lists:reverse([{Edge, Count}|Histo]);

build([H|T], Edge = [H1|_], Count, Histo) 
  when H =< H1 ->
    build(T, Edge, Count + 1, Histo);

build(L, [H|Edge], Count, Histo) ->
    build(L, Edge, 0, [{H, Count}|Histo]). 
    
