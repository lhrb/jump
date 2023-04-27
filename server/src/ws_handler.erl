-module(ws_handler).

-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, State) ->
    io:format("websocket connection initiated~n~p~n~nstate: ~p~n", [Req, State]),
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {[{text, <<"Hello!">>}], State}.

websocket_handle(Frame = {text, _}, State) ->
    io:format("websocket data from client: ~p~n", [Frame]),
    {[Frame], State};
websocket_handle(_Frame, State) ->
    io:format("websocket data from client: ~p~n", [_Frame]),
    {ok, State}.

websocket_info({log, Text}, State) ->
    {[{text, Text}], State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, Req, _State) ->
    io:format("websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
    ok.
