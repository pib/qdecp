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
-export([init/0, set/2, get/1, config/0, config/2]).

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

cache_key(Req, Config) ->
    %% Read the cookies (Cowboy lowecases the cookie names, which is bad)
    %% Cookie names are case-sensitive, dangit!
    {RawCookies, _} = cowboy_req:header(<<"cookie">>, Req),
    Cookies = qdecp_cookie:parse_cookies(RawCookies),

    Parts = proplists:get_value(key_parts, Config),
    KeyParts = lists:map(fun(Part) -> cache_key_parts(Req, Cookies, Part) end, Parts),

    lager:debug("Cache Key: ~p", [KeyParts]),
    iolist_to_binary(KeyParts).

cache_key_parts(Req, _Cookies, method) ->
    {Method, _} = cowboy_req:method(Req),
    Method;
cache_key_parts(Req, _Cookies, url) ->
    {Url, _} = cowboy_req:url(Req),
    Url;
cache_key_parts(_Req, Cookies, {cookie, Name}) ->
    proplists:get_value(Name, Cookies);
cache_key_parts(_Req, Cookies, {sub_cookie, CookieName, SubCookieName, Div}) ->
    case proplists:get_value(CookieName, Cookies) of
        undefined -> undefined;            
        Cookie ->
            SubCookies = qdecp_cookie:parse_sub_cookies(Cookie, Div),
            proplists:get_value(SubCookieName, SubCookies)
    end.

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