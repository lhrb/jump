-module(server_handler).

-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% Default body, replace with required behaviour 
    %% See https://ninenines.eu/docs/en/cowboy/2.6/guide/handlers/
   	Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Response body6 - replace me\n">>,
        Req0),
    io:format("req is: ~p~n:", [Req0]),
    {ok, Req, State}.
