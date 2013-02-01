%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@paulbonser.com>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% Takes requests, proxies them, and returns the result to the calling process.
%%% @end
%%% Created : 31 Jan 2013 by Paul Bonser <pib@paulbonser.com>
%%%-------------------------------------------------------------------
-module(proxy_server).

-behaviour(gen_server).

%% API
-export([start_link/0, proxy/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {request_pids=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Proxy an incoming cowboy request
%%
%% @spec proxy(cowboy_req:req(), ReplyTo :: pid()) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
proxy(Req, ReplyTo) ->
    gen_server:call(?SERVER, {proxy, Req, ReplyTo}).

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
handle_call({proxy, Req, ReplyTo}, _From, State) ->
    {Method, _} = cowboy_req:method(Req),
    {Url, _} = cowboy_req:url(Req),
    lager:info("~p ~p", [Method, Url]),
    case Method of
        <<"GET">> ->
            {ibrowse_req_id, ReqId} = ibrowse:send_req(binary_to_list(Url), [], get, [], 
                                                       [{stream_to, self()}]),
            {reply, {ok, ReqId}, add_req_pid(ReqId, ReplyTo, State)};
        _ ->
            {reply, {not_allowed, <<"GET">>}, State}
    end;
handle_call(Request, _From, State) ->
    {reply, {unexpected_request, Request}, State}.


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
handle_info({ibrowse_async_headers, ReqId, Code, Headers}, State) ->
    case get_req_pid(ReqId, State) of
        none -> ok;
        Pid ->
            Pid ! {proxy_headers, ReqId, list_to_integer(Code), Headers}
    end,
    {noreply, State};
handle_info({ibrowse_async_response, ReqId, Body}, State) ->
    case get_req_pid(ReqId, State) of
        none -> ok;
        Pid -> Pid ! {proxy_body, ReqId, Body}
    end,
    {noreply, State};
handle_info({ibrowse_async_response_end, ReqId}, State) ->
    case get_req_pid(ReqId, State) of
        none ->
            {noreply, State};
        Pid ->
            Pid ! {proxy_done, ReqId},
            {noreply, del_req_pid(ReqId, State)}
    end;
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
add_req_pid(ReqId, Pid, State) ->
    State#state{request_pids=[{ReqId, Pid} | State#state.request_pids]}.

get_req_pid(ReqId, State) ->
    case proplists:lookup(ReqId, State#state.request_pids) of
        {ReqId, Pid} -> Pid;
        none -> none
    end.

del_req_pid(ReqId, State) ->
    State#state{request_pids=proplists:delete(ReqId, State#state.request_pids)}.
