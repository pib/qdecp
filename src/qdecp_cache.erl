%%%-------------------------------------------------------------------
%%% @author Paul Bonser <pib@Qualee>
%%% @copyright (C) 2013, Paul Bonser
%%% @doc
%%% Cache handling for qdecp
%%% @end
%%% Created :  4 Feb 2013 by Paul Bonser <pib@Qualee>
%%%-------------------------------------------------------------------
-module(qdecp_cache).

%% API
-export([init/0, set/2, get/1]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    apply_all(init, [config()]).

set(Req, Response) ->
    {Method, _} = cowboy_req:method(Req),
    Config = config(),
    Key = cache_key(Req, Config),
    case Method of
        <<"GET">> ->
            lager:debug("Caching ~p ~p", [Key, Response]),
            apply_all(set, [Key, Response]);
        _ ->
            lager:debug("Not caching ~p ~p", [Key, Response])
    end.

get(Req) ->
    Key = cache_key(Req, config()),
    case apply_until(get, [Key]) of
        {ok, Cached} ->
            lager:debug("Response was in cache ~p ~p", [Key, Cached]),
            {ok, Cached};
        _ -> none
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
config() ->
    {ok, CacheConfig} = application:get_env(qdecp, cache),
    CacheConfig.

config(Key, Default) ->
    proplists:get_value(Key, config(), Default).

cache_key(Req, _Config) ->
    {Url, _} = cowboy_req:url(Req),
    Url.

apply_all(Fun, Args) ->
    lists:map(fun(Mod) -> apply(Mod, Fun, Args) end, config(modules, [])).

apply_until(Fun, Args) ->
    apply_until(config(modules, []), Fun, Args).

apply_until([], _Fun, _Args) ->
    none;
apply_until([Mod | Modules], Fun, Args) ->
    case apply(Mod, Fun, Args) of
        {ok, Val} -> {ok, Val};
        _ -> apply_until(Modules, Fun, Args)
    end.
