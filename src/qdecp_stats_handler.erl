-module(qdecp_stats_handler).
-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
 
init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    Stats = [{Key, integer_to_list(Val)} || {Key, Val} <- qdecp_stats:stats(today)],
    StatsJson = proplist_to_json(Stats),
    {ok, Req2} = cowboy_req:reply(200,
                                  [{<<"content-type">>, <<"application/json">>}],
                                  StatsJson, Req),
    {ok, Req2, State}.
 
terminate(_Reason, _Req, _State) ->
    ok.


proplist_to_json(Proplist) ->
    proplist_to_json(Proplist, ["}\n"]).

proplist_to_json([], [$, | Acc]) ->
    ["{" | Acc];
proplist_to_json([], Acc) ->
    ["{" | Acc];
proplist_to_json([{Key, Val} | Rest], Acc) ->
    proplist_to_json(Rest, [$,, $", Key, $", $:, $", Val, $" | Acc]).
