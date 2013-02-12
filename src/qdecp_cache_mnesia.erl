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
-export([init/1, get/1, set/2]).

-record(qdecp_cache, {key, value, created_at}).

%%%===================================================================
%%% API
%%%===================================================================
init(CacheConfig) ->
    MnesiaConfig = proplists:get_value(mnesia, CacheConfig, []),
    Nodes = proplists:get_value(nodes, MnesiaConfig, [node()]),
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(qdecp_cache, 
                        [{attributes, record_info(fields, qdecp_cache)},
                         {index, [created_at]}]).

set(Key, Value) ->
    {Today, _} = calendar:universal_time(),
    BinVal = term_to_binary(case should_gzip() of 
                                true -> {Today, {gzipped, zlib:gzip(term_to_binary(Value))}};
                                false -> {Today, {uncompressed, Value}}
                            end),
    Bucket = bucket(),
    execute_async(
      fun(P) ->
              Obj = riakc_obj:new(Bucket, Key, BinVal, "application/binary"),
              riakc_pb_socket:put(P, Obj)
      end),
    ok.

get(Key) ->
    Bucket = bucket(),
    Val = execute(
      fun(P) ->
              case riakc_pb_socket:get(P, Bucket, Key) of
                  {ok, Obj} ->
                      case catch riakc_obj:get_value(Obj) of
                          siblings -> throw(siblings);
                          no_value -> none;
                          Val -> Val
                      end;
                  _ -> none
              end
      end),
    {Today, _} = calendar:universal_time(),
    case binary_to_term(Val) of
        {Date, {gzipped, Zipped}} when Date =< Today -> binary_to_term(zlib:gunzip(Zipped));
        {Date, {uncompressed, Cached}} when Date =< Today -> Cached;
        _ ->
            execute_async(
              fun(P) -> riakc_pb_socket:delete(P, Bucket, Key) end),
            none
    end.


bucket() ->
    Config = qdecp_cache:config(riak, []),
    proplists:get_value(bucket, Config, <<"qdecp_cache">>).

should_gzip() ->
    Config = qdecp_cache:config(riak, []),
    proplists:get_value(gzip, Config, false).

execute(Fun) ->
    poolboy:transaction(qdecp_riak, fun(Worker) ->
        gen_server:call(Worker, {execute, Fun})
    end).

execute_async(Fun) ->
    spawn(fun() -> execute(Fun) end).
