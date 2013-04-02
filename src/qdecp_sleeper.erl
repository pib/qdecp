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
-export([start_link/0, add_sleep_time/1, is_sleeping/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sleep_timer=none}).

%%%===================================================================
%%% API
%%%===================================================================

%% Add sleep time in ms
-spec add_sleep_time(Time :: integer()) -> ok.
add_sleep_time(Time) ->
    gen_server:call(?SERVER, {add_sleep_time, Time}).

%% Check if currently sleeping
-spec is_sleeping() -> 'true' | 'false'.
is_sleeping() ->
    gen_server:call(?SERVER, {is_sleeping}).

-spec start_link() -> {ok, pid()} | ignore | {error, atom()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{sleep_timer=none}}.

-spec handle_call({add_sleep_time, integer}, any(), #state{}) -> {reply, ok, #state{}};
                 ({is_sleeping}, any(), #state{}) -> {reply, 'true' | 'false', #state{}}.
handle_call({add_sleep_time, Time}, _From, State=#state{sleep_timer=none}) ->
    Timer = erlang:start_timer(Time, ?SERVER, ok),
    {reply, ok, State#state{sleep_timer=Timer}};
handle_call({add_sleep_time, Time}, _From, State=#state{sleep_timer=Timer}) ->
    Timer2 = case erlang:cancel_timer(Timer) of
                 false -> %% Timer already done, start a new one
                     erlang:start_timer(Time, ?SERVER, ok);
                 TimeLeft ->
                     erlang:start_timer(Time + TimeLeft, ?SERVER, ok)
             end,
    {reply, ok, State#state{sleep_timer=Timer2}};

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
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
