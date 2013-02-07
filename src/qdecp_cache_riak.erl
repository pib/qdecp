%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@Qualee>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% Riak connection-pool-based cache
%%% @end
%%% Created :  5 Feb 2013 by Paul Bonser <pib@Qualee>
%%%-------------------------------------------------------------------
-module(qdecp_cache_riak).
-behavior(qdecp_cache_module).

%% API
-export([init/1, get/1, set/2]).

%%%===================================================================
%%% API
%%%===================================================================
init(CacheConfig) ->
    supervisor:start_child(
      qdecp_sup, {qdecp_cache_riak_sup,
                  {qdecp_cache_riak_sup, start_link, [CacheConfig]},
                  permanent, 5000, supervisor,
                  [qdecp_cache_riak_sup]}),
    ok.

set(Key, Value) ->
    BinVal = term_to_binary(Value),
    Bucket = bucket(),
    execute(
      fun(P) ->
              Obj = riakc_obj:new(Bucket, Key, BinVal, "application/binary"),
              riakc_pb_socket:put(P, Obj)
      end),
    ok.

get(Key) ->
    Bucket = bucket(),
    execute(
      fun(P) ->
              case riakc_pb_socket:get(P, Bucket, Key) of
                  {ok, Obj} ->
                      case catch riakc_obj:get_value(Obj) of
                          siblings -> throw(siblings);
                          no_value -> none;
                          Val -> binary_to_term(Val)
                      end;
                  _ -> none
              end
      end).

bucket() ->
    Config = qdecp_cache:config(riak, []),
    proplists:get_value(bucket, Config, <<"qdecp_cache">>).

execute(Fun) ->
    poolboy:transaction(qdecp_riak, fun(Worker) ->
        gen_server:call(Worker, {execute, Fun})
    end).
