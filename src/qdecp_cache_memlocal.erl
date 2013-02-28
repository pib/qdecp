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

%% API
-export([init/1, get/1, set/2]).

%%%===================================================================
%%% API
%%%===================================================================
init(_CacheConfig) ->
    cadfaerl:start_link(?CACHE, 1000),
    ok.

set(Key, Value) ->
    cadfaerl:put_ttl(?CACHE, Key, Value, 86400),
    ok.

get(Key) ->
    cadfaerl:get(?CACHE, Key).
