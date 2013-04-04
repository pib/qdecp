%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@Qualee>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% Clusterable mnesia-based cache
%%% @end
%%% Created : 11 Feb 2013 by Paul Bonser <pib@Qualee>
%%%-------------------------------------------------------------------
-module(qdecp_cache_mnesia).
-behavior(qdecp_cache_module).

%% API
-export([start_link/0, create_db/0, manage_db/1, init_cache/1, get/1, set/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TABLE, qdecp_cache).
-define(SLEEPER, qdecp_mnesia_sleeper).
-define(WRITEPOOL, qdecp_mnesia_write_workers).

-define(GET_TIMEOUT, 200).

-record(state, {}).
-record(qdecp_cache, {key, value, created_at}).

%%%===================================================================
%%% API
%%%===================================================================
create_db() ->
    MnesiaConfig = qdecp_cache:config(mnesia, []),
    manage_db(MnesiaConfig, [create_schema, start, create_tables]).

manage_db(Commands) ->
    MnesiaConfig = qdecp_cache:config(mnesia, []),
    manage_db(MnesiaConfig, Commands).

manage_db(_, []) ->
    ok;

manage_db(Config, [stop | Rest]) ->
    Nodes = proplists:get_value(nodes, Config, [node()]),
    %% Stop mnesia on each node
    lists:foreach(fun(Node) -> spawn(Node, mnesia, stop, []) end, Nodes),
    manage_db(Config, Rest);

manage_db(Config, [delete_schema | Rest]) ->
    Nodes = proplists:get_value(nodes, Config, [node()]),
    mnesia:delete_schema(Nodes),
    manage_db(Config, Rest);

manage_db(Config, [create_schema | Rest]) ->
    Nodes = proplists:get_value(nodes, Config, [node()]),
    mnesia:create_schema(Nodes),
    manage_db(Config, Rest);
    
manage_db(Config, [start | Rest]) ->
    Nodes = proplists:get_value(nodes, Config, [node()]),
    %% Start mnesia on each node
    lists:foreach(fun(Node) -> spawn(Node, mnesia, start, []) end, Nodes),
    manage_db(Config, [wait_for_mnesia | Rest]);

manage_db(Config, [wait_for_mnesia | Rest]) ->
    Nodes = proplists:get_value(nodes, Config, [node()]),
    %% Start mnesia on each node
    case length(Nodes) =:= length(mnesia:system_info(running_db_nodes)) of
        true -> manage_db(Config, Rest);
        false ->
            receive after 1000 -> ok end,
            manage_db(Config, [wait_for_mnesia | Rest])
    end;

manage_db(Config, [create_tables | Rest]) ->
    Nodes = proplists:get_value(nodes, Config, [node()]),
    Fragments = proplists:get_value(fragments, Config, 32),
    RamCopies = proplists:get_value(ram_copies, Config, 0),
    DiscCopies = proplists:get_value(disc_copies, Config, 0),
    DiscOnlyCopies = proplists:get_value(disc_only_copies, Config, 1),

    Res = mnesia:create_table(
            ?TABLE, 
            [{attributes, record_info(fields, qdecp_cache)},
             {index, [created_at]},
             {frag_properties, [
                                {hash_module, mnesia_frag_chash},
                                {node_pool, Nodes},
                                {n_fragments, Fragments},
                                {n_ram_copies, RamCopies},
                                {n_disc_copies, DiscCopies},
                                {n_disc_only_copies, DiscOnlyCopies}
                               ]}]),
    case Res of
        {atomic, ok} -> ok;
        {aborted, {already_exists, ?TABLE}} -> ok;
        Other -> throw(Other)
    end,
    manage_db(Config, Rest);

manage_db(Config, [delete_tables | Rest]) ->
    mnesia:delete_table(?TABLE),
    manage_db(Config, Rest);

manage_db(Config, [flush | Rest]) ->
    mnesia:activity(sync_dirty, fun() -> mnesia:clear_table(?TABLE) end, [], mnesia_frag),
    manage_db(Config, Rest).

init_cache(CacheConfig) ->
    MnesiaConfig = proplists:get_value(mnesia, CacheConfig, []),
    mnesia:start(),
    WritePoolArgs = [{name, {local, ?WRITEPOOL}},
                {size, proplists:get_value(read_pool_size, MnesiaConfig, 1)},
                {worker_module, qdecp_generic_worker}],
    supervisor:start_child(qdecp_sup, poolboy:child_spec(?WRITEPOOL, WritePoolArgs, [])),
    supervisor:start_child(qdecp_sup, {?SERVER, {?MODULE, start_link, []},
                                       permanent, 5000, worker, [?MODULE]}),

    ok.

do_set(Key, Value) ->
    Now = {Today, _} = calendar:universal_time(),
    Write = fun() ->
                    case catch mnesia:read(?TABLE, Key) of
                        [#qdecp_cache{created_at={Day, _Time}}] when Day =:= Today ->
                            ok; % This key is already up-to-date, don't re-write it
                        _ ->
                            catch mnesia:write(#qdecp_cache{key=Key, value=Value, created_at=Now})
                    end
            end,
    catch qdecp_generic_worker:async_call(
      ?WRITEPOOL,
      fun() -> catch mnesia:activity(sync_dirty, Write, [], mnesia_frag) end),
    ok.

do_get(Key, ReplyTo) ->
    {Today, _} = calendar:universal_time(),
    Read = fun() -> catch mnesia:read(?TABLE, Key) end,
    Res = case catch mnesia:activity(sync_dirty, Read, [], mnesia_frag) of
              [Cached=#qdecp_cache{created_at={Day, _Time}}] when Day =:= Today ->
                  lager:debug("Found match ~p, ~p", [Key, Today]),
                  {ok, Cached#qdecp_cache.value};
              [Cached=#qdecp_cache{created_at={Date, _Time}}] ->
                  lager:debug("Found old ~p, ~p =/= ~p", [Key, Date, Today]),
                  Delete = fun() -> catch mnesia:delete_object(Cached) end,
                  catch mnesia:activity(async_dirty, Delete, [], mnesia_frag),
                  none;
              _ ->
                  none
          end,
    ReplyTo ! Res.

%% Gen-server stuff
set(Key, Value) ->
    case qdecp_sleeper:is_sleeping(?SLEEPER) of
        true ->
            lager:warning("Sleeping, skipping setting key ~p", [Key]),
            ok;
        false ->
            gen_server:cast(?SERVER, {set, Key, Value}),
            ok
    end.

get(Key) ->
    case qdecp_sleeper:is_sleeping(?SLEEPER) of
        true -> 
            lager:warning("Sleeping, skipping getting key ~p", [Key]),
            none;
        false ->
            case catch gen_server:call(?SERVER, {get, Key, ?GET_TIMEOUT}) of
                {ok, Val} ->
                    {ok, Val};
                _ ->
                    none
            end
    end.

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

init(_Args) ->
    qdecp_sleeper:start_link(?SLEEPER),
    mnesia:subscribe(system),
    {ok, #state{}}.

handle_call({get, Key, Timeout}, From, State) ->
    %% Handle our own timeout to avoid spurious replies
    spawn(fun() ->
                  do_get(Key, self()),
                  Response = receive
                                 Res -> Res
                             after
                                 Timeout -> none
                             end,
                  gen_server:reply(From, Response)
          end),
                  
    {noreply, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({set, Key, Value}, State) ->
    do_set(Key, Value),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({mnesia_overload, Details}, State) ->
    {ok, SleepTime} = qdecp_sleeper:add_sleep_time(?SLEEPER, 5000),
    lager:warning("Mnesia overloaded, added sleep time (total now ~p): ~p", [SleepTime, Details]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    mnesia:unsubscribe(system),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

