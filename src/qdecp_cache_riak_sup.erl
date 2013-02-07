-module(qdecp_cache_riak_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(CacheConfig) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, CacheConfig).

init(CacheConfig) ->
    RiakConfig = proplists:get_value(riak, CacheConfig, []),
    Size = proplists:get_value(pool_size, RiakConfig, 10),
    Server = proplists:get_value(server, RiakConfig, {"127.0.0.1", 8087}),
    
    Name = qdecp_riak,
    PoolSpecs = [poolboy:child_spec(Name, [{name, {local, Name}},
                                           {worker_module, qdecp_cache_riak_worker},
                                           {size, Size}],
                                    Server)],
   
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
