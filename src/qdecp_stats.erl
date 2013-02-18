%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@Qualee>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% A server to track stats such as request counts, cache hits, misses, etc.
%%% @end
%%% Created : 15 Feb 2013 by Paul Bonser <pib@Qualee>
%%%-------------------------------------------------------------------
-module(qdecp_stats).

-behaviour(gen_server).

%% API
-export([start_link/0, log_event/1, stats/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TABLE, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
log_event(Event) ->
    gen_server:cast(?SERVER, {log_event, Event}).

stats(Which) ->
    gen_server:call(?SERVER, {get_stats, Which}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    mnesia:start(),
    case mnesia:create_table(?TABLE, [{disc_copies, [node()]}]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?TABLE}} -> ok;
        Else -> throw(Else)
    end,

    case mnesia:wait_for_tables([?TABLE], 20000) of
        {timeout, RemainingTabs} ->
            throw(RemainingTabs);
        ok ->
            ok
    end,
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_stats, all_keys}, _, State) ->
    Keys = mnesia:dirty_all_keys(?TABLE),
    {reply, Keys, State};
handle_call({get_stats, today}, _, State) ->
    {Today, _} = calendar:universal_time(),
    Match = mnesia:dirty_match_object({?TABLE, {Today, '_'}, '_'}),
    Stats = [{Key, Count} || {?TABLE, {_, Key}, Count} <- Match],
    {reply, Stats, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({log_event, Event}, State) ->
    {Today, _} = calendar:universal_time(),
    log_event_parts(Today, lists:reverse(tuple_to_list(Event))),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
log_event_parts(Today, Parts=[_Part | Rest]) ->
    SubEvent = string:join(lists:reverse(lists:map(fun erlang:atom_to_list/1, Parts)), "_"),
    mnesia:dirty_update_counter(?TABLE, {Today, SubEvent}, 1),
    log_event_parts(Today, Rest);
log_event_parts(_, []) ->
    ok.

