-module(qdecp_cache_riak_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([CacheConfig]) ->
    RiakConfig = proplists:get_value(riak, CacheConfig, []),
    Pools = proplists:get_value(connections, RiakConfig),
    
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, qdecp_cache_riak_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
