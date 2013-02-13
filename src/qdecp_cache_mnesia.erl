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
-export([create_db/0, init/1, get/1, set/2]).

-record(qdecp_cache, {key, value, created_at}).

%%%===================================================================
%%% API
%%%===================================================================
create_db() ->
    MnesiaConfig = qdecp_cache:config(mnesia, []),
    Nodes = proplists:get_value(nodes, MnesiaConfig, [node()]),
    Fragments = proplists:get_value(fragments, MnesiaConfig, 25),
    RamCopies = proplists:get_value(ram_copies, MnesiaConfig, 0),
    DiscCopies = proplists:get_value(disc_copies, MnesiaConfig, 0),
    DiscOnlyCopies = proplists:get_value(disc_only_copies, MnesiaConfig, 1),
    
    mnesia:create_schema(Nodes),

    mnesia:subscribe(system),
    mnesia:start(),
    mnesia:unsubscribe(system),

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
    end.

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
    spawn(fun() -> mnesia:dirty_write(#qdecp_cache{key=Key, value=Value, created_at=Now}) end),
    ok.

get(Key) ->
    {Today, _} = calendar:universal_time(),
    case mnesia:dirty_read(qdecp_cache, Key) of
        [Cached=#qdecp_cache{created_at={Day, _Time}}] when Day =:= Today ->
            Cached#qdecp_cache.value;
        [Cached=#qdecp_cache{}] ->
            spawn(fun() -> mnesia:dirty_delete_object(Cached) end),
            none;
        _ ->
            none
    end.
