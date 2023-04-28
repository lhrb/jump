-module(broadcast).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([add_player/1, remove_player/1, send_action/2, list_player/0, send/2, loop/0]).

-define(SERVER, ?MODULE).

-record(state, {player=[]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({add, Player}, _From, State) ->
    io:format("add new player: ~p~n", [Player]),
    NewPlayerList = lists:append(State#state.player, [Player]),
    NewState = State#state{player=NewPlayerList},
    {reply, {ok, NewState}, NewState};

handle_call({remove, Player}, _From, State) ->
    io:format("remove player: ~p~n", [Player]),
    NewPlayerList = lists:delete(Player, State#state.player),
    NewState = State#state{player=NewPlayerList},
    {reply, {ok, NewState}, NewState};

handle_call(list, _From, State) ->
    {reply, State#state.player, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send_action, Player, Action}, State) ->
    io:format("from ~p send ~p~n", [Player, Action]),
    Other = lists:delete(Player, State#state.player),
    ok = send(Other, Action),
    {noreply, State};

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

% sync
add_player(Player) -> gen_server:call(?SERVER, {add, Player}).
remove_player(Player) -> gen_server:call(?SERVER, {remove, Player}).
list_player() -> gen_server:call(?SERVER, list).

% async
send_action(Player, Action) -> gen_server:cast(?SERVER, {send_action, Player, Action}).

send([Pid|T], Message) ->
    Pid ! Message,
    send(T, Message);

send([], _) -> ok.

% debug helper
loop() ->
    receive
        Any ->
            io:format("~p: received message: ~p~n", [self(), Any]),
            loop()
    end.
