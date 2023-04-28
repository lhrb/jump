-module(broadcast).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([add_player/1, remove_player/1, send_action/2, list_player/0]).

-define(SERVER, ?MODULE).

-record(state, {player=[]}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({add, Player}, _From, State) ->
    NewPlayerList = lists:append(State#state.player, [Player]),
    NewState = State#state{player=NewPlayerList},
    {reply, {ok, NewState}, NewState};

handle_call({remove, Player}, _From, State) ->
    NewPlayerList = lists:delete(Player, State#state.player),
    NewState = State#state{player=NewPlayerList},
    {reply, {ok, NewState}, NewState};

handle_call(list, _From, State) ->
    {reply, State#state.player, State};

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


add_player(Player) -> gen_server:call(?SERVER, {add, Player}).
remove_player(Player) -> gen_server:call(?SERVER, {remove, Player}).
send_action(Player, Action) -> gen_server:call(?SERVER, {send_action, Player, Action}).
list_player() -> gen_server:call(?SERVER, list).
