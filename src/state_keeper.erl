%%%-------------------------------------------------------------------
%%% @author maxvel <>
%%% @copyright (C) 2015, maxvel
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2015 by maxvel <>
%%%-------------------------------------------------------------------
-module(state_keeper).

-behaviour(gen_server).

%% API
-export([start_link/0, save/2, restore/1, contains/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec save(any(), any()) -> ok.
save(Key, State) ->
    gen_server:call(?SERVER, {save, Key, State}).

-spec restore(any()) -> {ok, any()} | {error, not_found}.
restore(Key) ->
    case ets:lookup(?TAB, Key) of
        [{Key, Val}] -> {ok, Val};
        _ -> {error, not_found}
    end.

-spec contains(any()) -> boolean().
contains(Key) ->
    ets:member(?TAB, Key).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    er:get_or_new_e(?TAB, [named_table, protected]),
    er:obtain_e(?TAB),
    {ok, #state{}, hibernate}.

handle_call({save, Key, State}, _From, State) ->
    ets:insert(?TAB, {Key, State}),
    {reply, ok, State, hibernate};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
