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
-export([create_db/0, manage_db/1, init/1, get/1, set/2]).

-define(TABLE, qdecp_cache).
-define(POOL, qdecp_mnesia_workers).
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

init(CacheConfig) ->
    MnesiaConfig = proplists:get_value(mnesia, CacheConfig, []),
    mnesia:start(),
    PoolArgs = [{name, {local, ?POOL}},
                {size, proplists:get_value(write_pool_size, MnesiaConfig, 5)},
                {worker_module, qdecp_generic_worker}],
    supervisor:start_child(qdecp_sup, poolboy:child_spec(?POOL, PoolArgs, [])),
    ok.

set(Key, Value) ->
    Now = {Today, _} = calendar:universal_time(),
    Write = fun() ->
                    case mnesia:read(?TABLE, Key) of
                        [#qdecp_cache{created_at={Day, _Time}}] when Day =:= Today ->
                            ok; % This key is already up-to-date, don't re-write it
                        _ ->
                            mnesia:write(#qdecp_cache{key=Key, value=Value, created_at=Now})
                    end
            end,
    qdecp_generic_worker:async_call(
      ?POOL,
      fun() -> mnesia:activity(sync_dirty, Write, [], mnesia_frag) end),
    ok.

get(Key) ->
    {Today, _} = calendar:universal_time(),
    Read = fun() -> mnesia:read(?TABLE, Key) end,
    case mnesia:activity(sync_dirty, Read, [], mnesia_frag) of
        [Cached=#qdecp_cache{created_at={Day, _Time}}] when Day =:= Today ->
            lager:debug("Found match ~p, ~p", [Key, Today]),
            {ok, Cached#qdecp_cache.value};
        [Cached=#qdecp_cache{created_at={Date, _Time}}] ->
            lager:debug("Found old ~p, ~p =/= ~p", [Key, Date, Today]),
            Delete = fun() -> mnesia:delete_object(Cached) end,
            mnesia:activity(async_dirty, Delete, [], mnesia_frag),
            none;
        _ ->
            none
    end.
