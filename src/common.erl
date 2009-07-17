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

-module(common).

-export([random_token/0]).

random_token() ->
    Term = term_to_binary({node(), make_ref()}),
    Digest = erlang:md5(Term),
    binary_to_hex(Digest).
 
binary_to_hex(Bin) when is_binary(Bin) ->
    [oct_to_hex(N) || <<N:4>> <= Bin].

oct_to_hex(0) -> $0;
oct_to_hex(1) -> $1;
oct_to_hex(2) -> $2;
oct_to_hex(3) -> $3;
oct_to_hex(4) -> $4;
oct_to_hex(5) -> $5;
oct_to_hex(6) -> $6;
oct_to_hex(7) -> $7;
oct_to_hex(8) -> $8;
oct_to_hex(9) -> $9;
oct_to_hex(10) -> $a;
oct_to_hex(11) -> $b;
oct_to_hex(12) -> $c;
oct_to_hex(13) -> $d;
oct_to_hex(14) -> $e;
oct_to_hex(15) -> $f.
