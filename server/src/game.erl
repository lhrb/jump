-module(game).

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([send/1]).

-define(SERVER, ?MODULE).

-record(state, {port=undefined}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, "node main.js"}, []),
    {ok, #state{port=Port}}.

handle_call({send, Str}, _From, State) ->
    io:format("send: ~p~n", [Str]),
    Port = State#state.port,
    Port ! {self(), {command, Str}},
    receive
        {Port, {data, Data}} ->
            io:format("received: ~p~n", [Data])
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

send(Str) -> gen_server:call(?SERVER, {send, Str}).
