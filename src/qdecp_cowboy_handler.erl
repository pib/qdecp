-module(qdecp_cowboy_handler).
-behavior(cowboy_loop_handler).
-export([init/3, info/3, terminate/3]).

-record(state, {req_id, res_code, res_headers, res_body=[]}).

init({tcp, http}, Req, _) ->
    {Method, _} = cowboy_req:method(Req),
    qdecp_stats:log_event({request_in, list_to_atom(string:to_lower(binary_to_list(Method)))}),
    case qdecp_cache:get(Req) of
        {ok, {Code, Headers, Body}} ->
            self() ! {ibrowse_async_headers, none, Code, Headers},
            self() ! {ibrowse_async_response, none, Body},
            self() ! {ibrowse_async_response_end, none},
            {loop, Req, #state{req_id=none}};
        none ->
            start_request(Req)
    end.

start_request(Req) ->
    {Method, _} = cowboy_req:method(Req),
    {Url, _} = cowboy_req:url(Req),
    Headers = request_headers(Req),
    [Socket] = cowboy_req:get([socket], Req),
    {ok, {Ip, _Port}} = inet:sockname(Socket),
    lager:debug("~p ~p ~p", [Ip, Method, Url]),
    case Method of
        <<"GET">> ->
            case send_req(Ip, get, Url, Headers, [], 10) of
                {ok, ReqId} ->
                    {loop, Req, #state{req_id=ReqId}, hibernate};
                {error, Code} ->
                    {ok, Req2} = cowboy_req:reply(Code, [], Req),
                    {shutdown, Req2, #state{}}
            end;
                    
        <<"POST">> ->
            {ok, Body, _} = cowboy_req:body(Req),
            case send_req(Ip, post, Url, Headers, Body, 10) of
                 {ok, ReqId} ->
                    {loop, Req, #state{req_id=ReqId}, hibernate};
                {error, Code} ->
                    {ok, Req2} = cowboy_req:reply(Code, [], Req),
                    {shutdown, Req2, #state{}}
            end;
        <<"CONNECT">> ->
            {upgrade, protocol, qdecp_connect_proxy};
        _ ->
            {ok, Req2} = cowboy_req:reply(405, [{<<"allow">>, <<"GET, POST, CONNECT">>}], Req),
            qdecp_stats:log_event({response, '405'}),
            {shutdown, Req2, #state{}}
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
        _ -> lager:debug("Non-200 response: ~p", [State#state.res_code]),
             lager:debug("Non-200 response body: ~p", [State#state.res_body])
    end,
    case State#state.req_id of
        ReqId ->
            %% The request id matches, so we're good
            Body = lists:reverse(State#state.res_body),
            Code = list_to_integer(State#state.res_code),
            Headers = response_headers(State#state.res_headers),
            qdecp_cache:set(Req, {State#state.res_code, Headers, Body}),
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            qdecp_stats:log_event({response, list_to_atom(State#state.res_code)}),
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

request_headers(Req) ->
    {Headers, _} = cowboy_req:headers(Req),
    [{binary_to_list(Name), binary_to_list(Val)} || {Name, Val} <- Headers,
                                                    Name =/= <<"host">>].

response_headers(Headers = [{Name, _} | _]) when is_binary(Name) ->
    Headers;
response_headers(Headers) ->
    lager:debug("Response headers: ~p", [Headers]),
    [{list_to_binary(string:to_lower(Name)), list_to_binary(Val)} ||
        {Name, Val} <- Headers, Name =/= "Content-Length",
        Name =/= "Transfer-Encoding"].

send_req(_, _, Url, _, _, 0) ->
    lager:error("Too many retries, giving up on connection to ~p", [Url]),
    {error, 503};
send_req(Ip, Method, Url, Headers, Body, Retries) ->
    case ibrowse:send_req(binary_to_list(Url), Headers, Method, Body,
                          [{stream_to, self()},{socket_options, [{ip, Ip}]}]) of
        {ibrowse_req_id, ReqId} -> 
            qdecp_stats:log_event({request_out, Method}),
            {ok, ReqId};
        {error, retry_later} ->
            qdecp_stats:log_event({request_retry, Method}),
            lager:error("Got retry later for ~p, retrying.", [Url]),
            send_req(Ip, Method, Url, Headers, Body, Retries - 1);
        {error, {conn_failed, {error, nxdomain}}} ->
            lager:error("Connection to ~p failed, domain does not exist", [Url]),
            {error, 502};
        {error, {conn_failed, Err}} ->
            qdecp_stats:log_event({request_retry, Method}),
            lager:error("Connection to ~p failed with ~p, retrying", [Url, Err]),
            send_req(Ip, Method, Url, Headers, Body, Retries - 1);
        Else ->
            lager:error("Failed to send request to ~p: ~p", [Url, Else]),
            {error, 500}
    end.
