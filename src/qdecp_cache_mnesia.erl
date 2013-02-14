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
    manage_db(Config, Rest);

manage_db(Config, [create_tables | Rest]) ->
    Nodes = proplists:get_value(nodes, Config, [node()]),
    Fragments = proplists:get_value(fragments, Config, 32),
    RamCopies = proplists:get_value(ram_copies, Config, 0),
    DiscCopies = proplists:get_value(disc_copies, Config, 0),
    DiscOnlyCopies = proplists:get_value(disc_only_copies, Config, 1),

    Res = mnesia:create_table(
            qdecp_cache, 
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
        {aborted, {already_exists, qdecp_cache}} -> ok;
        Other -> throw(Other)
    end,
    manage_db(Config, Rest);

manage_db(Config, [delete_tables | Rest]) ->
    mnesia:delete_table(qdecp_cache),
    manage_db(Config, Rest).

init(_CacheConfig) ->
    mnesia:start(),
    case mnesia:wait_for_tables([qdecp_cache], 20000) of
        {timeout, RemainingTabs} ->
            throw(RemainingTabs);
        ok ->
            ok
    end.

set(Key, Value) ->
    Now = calendar:universal_time(),
    Write = fun() -> mnesia:write(#qdecp_cache{key=Key, value=Value, created_at=Now}) end,
    mnesia:activity(async_dirty, Write, [], mnesia_frag),
    ok.

get(Key) ->
    {Today, _} = calendar:universal_time(),
    Read = fun() -> mnesia:read(qdecp_cache, Key) end,
    case mnesia:activity(sync_dirty, Read, [], mnesia_frag) of
        [Cached=#qdecp_cache{created_at={Day, _Time}}] when Day =:= Today ->
            Cached#qdecp_cache.value;
        [Cached=#qdecp_cache{}] ->
            Delete = fun() -> mnesia:delete_object(Cached) end,
            mnesia:activity(async_dirty, Delete, [], mnesia_frag),
            none;
        _ ->
            none
    end.
