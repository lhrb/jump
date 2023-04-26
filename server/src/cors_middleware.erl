-module(cors_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    io:format("~p request~n", [cowboy_req:method(Req)]),
    Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req),
    {ok, Req1, Env}.
