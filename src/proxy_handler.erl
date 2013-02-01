-module(proxy_handler).
-behavior(cowboy_loop_handler).
-export([init/3, info/3, terminate/3]).

proxy_request(Req) ->
    {Method, _} = cowboy_req:method(Req),
    {Host, _} = cowboy_req:host_url(Req),
    {Url, _} = cowboy_req:url(Req),
    lager:info("~p ~p", [Method, Url]),
    self() ! {reply, "OK"}.

init({tcp, http}, Req, _) ->
    proxy_request(Req),
    {loop, Req, undefined, 600, hibernate}.

info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, State};
info(Message, Req, State) ->
    {loop, Req, State, hibernate}.


terminate(Reason, Req, State) ->
    lager:info("Terminating request ~p", [Reason]),
    ok.

