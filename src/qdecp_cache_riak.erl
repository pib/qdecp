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
init(_CacheConfig) ->
    ok.

set(Key, Value) ->
    Bucket = bucket(),
    riakc:execute(
      fun(P) ->
              Obj = riak_obj:new(Bucket, Key, Value, "application/binary"),
              riak_pb_socket:put(P, Obj)
      end),
    ok.

get(Key) ->
    Bucket = bucket(),
    riakc:execute(
      fun(P) ->
              case riak_pb_socket:get(P, Bucket, Key) of
                  {ok, Obj} ->
                      case catch riak_obj:get_value(Obj) of
                          siblings -> throw(siblings);
                          no_value -> none;
                          Val -> Val
                      end;
                  _ -> none
              end
      end).

bucket() ->
    Config = qdecp_cache:config(riak, []),
    proplists:get_value(bucket, Config, <<"qdecp_cache">>).
