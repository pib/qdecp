%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@Qualee>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% A server to act as a notifier that it's time to take a break.
%%% @end
%%% Created : 28 Mar 2013 by Paul Bonser <pib@Qualee>
%%%-------------------------------------------------------------------
-module(qdecp_sleeper).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, add_sleep_time/1, add_sleep_time/2,
         is_sleeping/0, is_sleeping/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {name, sleep_timer=none}).

%%%===================================================================
%%% API
%%%===================================================================

%% Add sleep time in ms
-spec add_sleep_time(Time :: integer()) -> ok.
add_sleep_time(Time) ->
    add_sleep_time(?SERVER, Time).

-spec add_sleep_time(Name :: atom(), Time :: integer()) -> ok.
add_sleep_time(Name, Time) ->
    gen_server:call(Name, {add_sleep_time, Time}).

%% Check if currently sleeping
-spec is_sleeping() -> 'true' | 'false'.
is_sleeping() ->
    is_sleeping(?SERVER).

-spec is_sleeping(Name :: atom()) -> 'true' | 'false'.
is_sleeping(Name) ->
    gen_server:call(Name, {is_sleeping}).

-spec start_link() -> {ok, pid()} | ignore | {error, atom()}.
start_link() ->
    start_link(?SERVER).

-spec start_link(Name :: atom()) -> {ok, pid()} | ignore | {error, atom()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([Name :: atom()]) -> {ok, #state{}}.
init([Name]) ->
    {ok, #state{name=Name, sleep_timer=none}}.

-spec handle_call({add_sleep_time, integer()}, any(), #state{}) -> {reply, {ok, integer()}, #state{}};
                 ({is_sleeping}, any(), #state{}) -> {reply, 'true' | 'false', #state{}}.
handle_call({add_sleep_time, Time}, _From, State=#state{name=Name, sleep_timer=none}) ->
    Timer = erlang:start_timer(Time, Name, ok),
    {reply, {ok, Time}, State#state{sleep_timer=Timer}};
handle_call({add_sleep_time, Time}, _From, State=#state{name=Name, sleep_timer=Timer}) ->
    Time2 = case erlang:cancel_timer(Timer) of
                false -> %% Timer already done, start a new one
                    Time;
                TimeLeft ->
                    Time + TimeLeft
            end,
    Timer2 = erlang:start_timer(Time2, Name, ok),
    {reply, {ok, Time2}, State#state{sleep_timer=Timer2}};

handle_call({is_sleeping}, _From, State=#state{sleep_timer=none}) ->
    {reply, false, State};
handle_call({is_sleeping}, _From, State) ->
    {reply, true, State}.

-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info({timeout, integer(), ok} | any(), #state{}) -> {noreply, #state{}}.
handle_info({timeout, T1, ok}, State=#state{sleep_timer=T2}) when T1 =:= T2 ->
    {noreply, State#state{sleep_timer=none}};
handle_info(Info, State) ->
    lager:warning("Unexpected handle_info: ~p, ~p", [Info, State]),
    {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
