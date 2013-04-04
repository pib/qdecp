%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@Qualee>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% Local in-memory cache
%%% @end
%%% Created :  4 Feb 2013 by Paul Bonser <pib@Qualee>
%%%-------------------------------------------------------------------
-module(qdecp_cache_memlocal).
-behavior(qdecp_cache_module).

-define(CACHE, qdecp_cache_memlocal).
-define(Q, qdecp_cache_memlocal_q).

%% API
-export([init_cache/1, get/1, set/2]).

%% Callback exports
-export([do_get/1, do_set/2]).

%%%===================================================================
%%% API
%%%===================================================================
init_cache(CacheConfig) ->
    MemConfig = proplists:get_value(memlocal, CacheConfig, []),
    Size = proplists:get_value(size, MemConfig, 5000),
    supervisor:start_child(qdecp_sup, {?CACHE,
                                       {cadfaerl, start_link, [?CACHE, Size]},
                                       permanent, 5000, worker, [cadfaerl]}),
    supervisor:start_child(qdecp_sup, {?Q,
                                       {qdecp_gfq, start_link, [?Q, ?MODULE]},
                                       permanent, 5000, worker, [qdecp_gfq]}),
    ok.

set(Key, Value) ->
    qdecp_gfq:set(?Q, Key, Value).

get(Key) ->
    qdecp_gfq:get(?Q, Key, 100).


%% Callback API
do_set(Key, Value) ->
    gen_server:call(?CACHE, {put, Key, Value, 86400}, infinity),
    ok.

do_get(Key) ->
    gen_server:call(?CACHE, {get, Key, undefined, undefined}, infinity).
