%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
%    {_Status2, SInterval} = application:get_env(server, stats_interval),
    Dispatch = cowboy_router:compile([
            {'_', [{"/", server_handler, []},
                   {"/websocket", ws_handler, []}]}
        ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{
          env => #{dispatch => Dispatch},
          middlewares => [cors_middleware, cowboy_router, cowboy_handler]
         }
        ),    

    server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
