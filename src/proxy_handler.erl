-module(proxy_handler).
-behavior(cowboy_loop_handler).
-export([init/3, info/3, terminate/3]).

-record(state, {req_id, res_code, res_headers, res_body=[]}).

init({tcp, http}, Req, _) ->
    case check_cache(Req) of
        {ok, {Code, Headers, Body}} ->
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            {ok, Req2, undefined_state};
        none ->
            start_request(Req)
    end.

start_request(Req) ->
    {Method, _} = cowboy_req:method(Req),
    {Url, _} = cowboy_req:url(Req),
    lager:debug("~p ~p", [Method, Url]),
    case Method of
        <<"GET">> ->
            {ok, ReqId} = send_req(Url, []),
            {loop, Req, #state{req_id=ReqId}, 5000, hibernate};
        _ ->
            {ok, Req2} = cowboy_req:reply(405, [{<<"allowed">>, <<"Get">>}], Req),
            {ok, Req2, undefined_state}
    end.

info({ibrowse_async_headers, ReqId, Code, Headers}, Req, State) ->
    case State#state.req_id of
        ReqId ->
            %% The request id matches, so we're good
            {loop, Req, State#state{res_code=Code, res_headers=Headers}, hibernate};
        _ ->
            %% Request id didn't match, ignore it
            lager:warning("Ignoring headers from different request id: ~p", [ReqId]),
            {loop, Req, State, hibernate}
    end;
info({ibrowse_async_response, ReqId, Body}, Req, State) ->
    case State#state.req_id of
        ReqId ->
            %% The request id matches, so we're good
            {loop, Req, State#state{res_body=[Body | State#state.res_body]}, hibernate};
        _ ->
            %% Request id didn't match, ignore it
            lager:warning("Ignoring response from different request id: ~p", [ReqId]),
            {loop, Req, State, hibernate}
    end;
info({ibrowse_async_response_end, ReqId}, Req, State) ->
    case State#state.res_code of
        "200" -> ok;
        _ -> lager:warning("Non-200 response: ~p, ~p", [State#state.res_code,
                                                        State#state.res_body])
    end,
    case State#state.req_id of
        ReqId ->
            %% The request id matches, so we're good
            Body = lists:reverse(State#state.res_body),
            Code = list_to_integer(State#state.res_code),
            Headers = lists:reverse(State#state.res_headers),
            cache(Req, {Code, Headers, Body}),
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            {ok, Req2, State};
        _ ->
            %% Request id didn't match, ignore it
            lager:warning("Ignoring response end from different request id: ~p", [ReqId]),
            {loop, Req, State, hibernate}
    end;

info(Message, Req, State) ->
    lager:info("Ignoring unrecognized message ~p", [Message]),
    {loop, Req, State, hibernate}.

terminate(Reason, _Req, State) ->
    case Reason of
        {normal, shutdown} ->
            lager:debug("Terminating request ~p with ~p", [State#state.req_id, Reason]);
        _ ->
            lager:info("Terminating request ~p with ~p", [State#state.req_id, Reason])
    end,
    ok.

cache(Req, Response) ->
    {Url, _} = cowboy_req:url(Req),
    lager:debug("Caching ~p ~p", [Url, Response]),
    cadfaerl:put(response_cache, Url, Response).

check_cache(Req) ->
    {Url, _} = cowboy_req:url(Req),
    case cadfaerl:get(response_cache, Url) of
        {ok, Cached} ->
            lager:debug("Response was in cache ~p ~p", [Url, Cached]),
            {ok, Cached};
        _ -> none
    end.

send_req(Url, Headers) ->
    case ibrowse:send_req(binary_to_list(Url), Headers, get, [], [{stream_to, self()}]) of
        {ibrowse_req_id, ReqId} -> {ok, ReqId};
        Error ->
            lager:error("Error sending request ~p", [Error]),
            send_req(Url, Headers)
    end.
