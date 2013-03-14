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
-define(POOL, qdecp_stats_write_pool).
-record(state, {in, in_count, processed, process_every}).

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
init(_Args) ->
    StatsConfig = case application:get_env(qdecp, stats) of
                      undefined -> [];
                      Other -> Other
                  end,

    ProcessEvery = proplists:get_value(process_every, StatsConfig, 1000000),

    {ok, #state{in=[], processed=dict:new(), process_every=ProcessEvery}}.

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
handle_call({get_stats, all_keys}, _, State=#state{in=In, processed=P}) ->
    Processed = process_events(In, P),
    {Today, _} = calendar:universal_time(),
    Day = case dict:find(Today, Processed) of
              {ok, D} -> D;
              error -> dict:new()
          end,
    Keys = dict:fetch_keys(Day),
    {reply, Keys, State#state{in=[], processed=Processed}};

handle_call({get_stats, today}, _, State=#state{in=In, processed=P}) ->
    Processed = process_events(In, P),
    {Today, _} = calendar:universal_time(),
    Day = case dict:find(Today, Processed) of
              {ok, D} -> D;
              error -> dict:new()
          end,
    Stats = dict:to_list(Day),
    {reply, Stats, State#state{in=[], processed=Processed}};

handle_call({clear}, _, State) ->
    {reply, ok, State#state{in=[], processed=dict:new()}};

handle_call(Request, From, State) ->
    lager:warning("Unrecognized call ~p from ~p with state ~p", [Request, From, State]),
    {reply, ok, State}.

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
handle_cast({log_event, Event}, State=#state{in=In, in_count=InCount, process_every=PE}) ->
    {Today, _} = calendar:universal_time(),
    NewCount = case InCount >= PE of
                   true ->
                       async_process(),
                       0;
                   false -> InCount + 1
               end,
    {noreply, State#state{in=[{Today, Event} | In], in_count=NewCount}};
handle_cast(Msg, State) ->
    lager:warning("Unrecognized cast ~p with state ~p", [Msg, State]),
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
handle_info(Info, State) ->
    lager:warning("Unrecognized message ~p with state ~p", [Info, State]),
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
event_parts(Acc, Parts) when is_tuple(Parts) ->
    event_parts(Acc, lists:reverse(tuple_to_list(Parts)));
event_parts(Acc, Parts=[_Part | Rest]) ->
    SubEvent = string:join(lists:reverse(lists:map(fun erlang:atom_to_list/1, Parts)), "_"),
    event_parts([SubEvent | Acc], Rest);
event_parts(Acc, []) ->
    Acc.

process_events(In, Processed) ->
    Processed2 = lists:foldl(
                   fun({Date, Event}, EventDays) ->
                           OldDay = case dict:find(Date, EventDays) of
                                        {ok, Events} -> Events;
                                        error -> dict:new()
                                    end,
                           NewDay = lists:foldl(
                                      fun(EventPart, DayEvents) ->
                                              dict:update_counter(EventPart, 1, DayEvents)
                                      end, OldDay, event_parts([], Event)),
                           dict:store(Date, NewDay, EventDays)
                   end, Processed, In),

    {Today, _} = calendar:universal_time(),
    dict:filter(fun(Date, _) -> Date =:= Today end, Processed2).

async_process() ->
    spawn(fun() -> ?MODULE:stats(today) end).
