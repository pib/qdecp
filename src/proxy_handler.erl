-module(proxy_handler).
-behavior(cowboy_loop_handler).
-export([init/3, info/3, terminate/3]).

init({tcp, http}, Req, _) ->
    proxy_server:proxy(Req, self()),
    {loop, Req, undefined, 600, hibernate}.

info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
    {ok, Req2, State};
info({not_allowed, Allowed}, Req, State) ->
    {ok, Req2} = cowboy_req:reply(405, [{<<"allowed">>, Allowed}], Req),
    {ok, Req2, State};
info(Message, Req, State) ->
    {loop, Req, State, hibernate}.


terminate(Reason, Req, State) ->
    lager:info("Terminating request ~p", [Reason]),
    ok.

