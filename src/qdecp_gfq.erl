%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@Qualee>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% Get-Favoring Queue, queues get and set requests, favors the gets
%%% @end
%%% Created :  7 Mar 2013 by Paul Bonser <pib@Qualee>
%%%-------------------------------------------------------------------
-module(qdecp_gfq).

-behaviour(gen_server).

%% API
-export([start_link/2, get/2, get/3, set/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Worker callbacks
-export([worker_loop/2]).

-record(state, {name, get_set_mod, worker, getq, getdict, setq}).

%%%===================================================================
%%% API
%%%===================================================================
get(Name, Key) ->
    get(Name, Key, infinity).

get(Name, Key, Timeout) ->
    case catch gen_server:call(Name, {get, Key}, Timeout) of
        {'EXIT', {timeout, _}} ->
            none;
        Else -> Else
    end.

set(Name, Key, Val) ->
    gen_server:cast(Name, {set, Key, Val}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, GetSetMod) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, GetSetMod], []).

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
init([Name, GetSetMod]) ->
    process_flag(trap_exit, true),
    WorkerPid = spawn_link(?MODULE, worker_loop, [Name, GetSetMod]),
    {ok, #state{name=Name, get_set_mod=GetSetMod,
                worker={WorkerPid, noref},
                getq=queue:new(), getdict=dict:new(),
                setq=queue:new()}}.

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
handle_call({get_job}, From, State=#state{getq=GetQ, setq=SetQ, worker={WPid, _}}) ->
    case {queue:peek(GetQ), queue:peek(SetQ)} of
        {empty, empty} ->
            {noreply, State#state{worker={WPid, From}}};
        {{value, GetKey}, _} ->
            {reply, {get, GetKey}, State#state{worker={WPid, noref}}};
        {empty, {value, {SetKey, SetVal}}} ->
            {reply, {set, SetKey, SetVal}, State#state{worker={WPid, noref}}}
    end;

handle_call({get, Key}, From, State=#state{getq=GetQ, getdict=GetDict,
                                           worker={_, noref}}) ->
    NewGetQ = queue:in(Key, GetQ),
    NewGetDict = dict:append(Key, From, GetDict),
    {noreply, State#state{getq=NewGetQ, getdict=NewGetDict}};

handle_call({get, Key}, From, State=#state{worker={WPid, WRef}}) ->
    gen_server:reply(WRef, {get, Key}),
    handle_call({get, Key}, From, State#state{worker={WPid, noref}});
    
handle_call(Request, From, State) ->
    lager:warning("Unhandled call: ~p from ~p with state ~p", [Request, From, State]),
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
handle_cast({get_done, Key, Value}, State=#state{getq=GetQ, getdict=GetDict}) ->
    NewGetQ = queue:filter(fun(Item) -> Item =/= Key end, GetQ),
    NewGetDict = dict:erase(Key, GetDict),
    ToReply = case dict:find(Key, GetDict) of
                  {ok, Refs} -> Refs;
                  error -> []
              end,
    lists:foreach(fun(Ref) ->
                          gen_server:reply(Ref, Value)
                  end, ToReply),
    {noreply, State#state{getq=NewGetQ, getdict=NewGetDict}};

handle_cast({set_done, _}, State=#state{setq=SetQ}) ->
    {noreply, State#state{setq=queue:drop(SetQ)}};

handle_cast({set, Key, Value}, State=#state{setq=SetQ, worker={_, noref}}) ->
    {noreply, State#state{setq=queue:in({Key, Value}, SetQ)}};

handle_cast({set, Key, Value}, State=#state{worker={WPid, WRef}}) ->
    gen_server:reply(WRef, {set, Key, Value}),
    handle_cast({set, Key, Value}, State#state{worker={WPid, noref}});

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast: ~p with state ~p", [Msg, State]),
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
handle_info({'EXIT', From, Reason}, State=#state{name=Name, 
                                                 get_set_mod=GetSetMod,
                                                 worker={WPid, _}}
           ) when From =:= WPid ->
    lager:warning("gfq child ~p for ~p exited with reason ~p", [WPid, GetSetMod, Reason]),
    WorkerPid = spawn_link(?MODULE, worker_loop, [Name, GetSetMod]),
    {noreply, State#state{worker={WorkerPid, noref}}};

handle_info(Info, State) ->
    lager:warning("Unhandled message: ~p with state ~p", [Info, State]),
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
%% @docpp
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

worker_loop(Name, GetSetMod) ->
    case gen_server:call(Name, {get_job}, infinity) of
        {get, Key} ->
            Value = (catch GetSetMod:do_get(Key)),
            gen_server:cast(Name, {get_done, Key, Value});
        {set, Key, Value} ->
            Result = (catch GetSetMod:do_set(Key, Value)),
            gen_server:cast(Name, {set_done, Key})
    end,
    worker_loop(Name, GetSetMod).
