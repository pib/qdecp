-module(qdecp_cowboy_handler).
-behavior(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-record(state, {http_client, timeout_ms, retries, retry_wait}).

init({tcp, http}, Req, [HttpConfig]) ->
    HttpClient = proplists:get_value(client, HttpConfig, qdecp_client_lhttpc),
    Retries = proplists:get_value(retries, HttpConfig, 10),
    RetryWait = proplists:get_value(retry_wait, HttpConfig, 500),
    Timeout = proplists:get_value(timeout, HttpConfig, 5000),
    {ok, Req, #state{http_client=HttpClient, timeout_ms=Timeout,
                     retries=Retries, retry_wait=RetryWait}}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    qdecp_stats:log_event({request_in, list_to_atom(string:to_lower(binary_to_list(Method)))}),
    case qdecp_cache:get(Req) of
        {ok, {Code, Headers, Body}} ->
            qdecp_cache:set(Req, {Code, Headers, Body}),
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            qdecp_stats:log_event({response, list_to_atom(integer_to_list(Code))}),
            {ok, Req2, State};
        none ->
            case catch do_request(Req, State) of
                {error, Where, Err} ->
                    lager:error("Error ~p: ~p", [Where, Err]),
                    {ok, Req2} = cowboy_req:reply(500, [], "Something went horribly wrong.", Req),
                    {ok, Req2, State};
                Response ->
                    Response
            end
    end.

do_request(Req, State=#state{http_client=HttpClient}) ->
    {ReqMethod, _} = cowboy_req:method(Req),
    {Url, _} = cowboy_req:url(Req),
    ReqHeaders = clean_request_headers(Req),
    [Socket] = cowboy_req:get([socket], Req),
    Ip = case inet:sockname(Socket) of
             {ok, {IpAddr, _Port}} -> IpAddr;
             {error, Err} ->
                 throw({error, "Getting IP from socket", Err})
         end,

    {{PeerIp, _}, _} = cowboy_req:peer(Req),
    lager:debug("~p ~p ~p from ~p", [Ip, ReqMethod, Url, PeerIp]),
    case PeerIp == Ip of
        true-> throw({error, "Stopping recursive request", "Peer IP is our IP!"});
        _ -> ok
    end,
                
    Reply = case ReqMethod of
                <<"GET">> ->
                    HttpClient:request(Ip, get, Url, ReqHeaders, [], 10, 5000, 500);
                <<"POST">> ->
                    {ok, ReqBody, _} = cowboy_req:body(Req),
                    HttpClient:request(Ip, post, Url, ReqHeaders, ReqBody, 10, 5000, 500);
                <<"CONNECT">> ->
                    {upgrade, protocol, qdecp_connect_proxy};
                _ ->
                    {ok, 405, [{<<"allow">>, <<"GET, POST, CONNECT">>}], []}
            end,

    case Reply of
        {ok, Code, Headers, Body} ->
            CleanHeaders = clean_response_headers(Headers),
            qdecp_cache:set(Req, {Code, CleanHeaders, Body}),
            {ok, Req2} = cowboy_req:reply(Code, CleanHeaders, Body, Req),
            qdecp_stats:log_event({response, list_to_atom(integer_to_list(Code))}),
            {ok, Req2, State};
        {error, Code} ->
            {ok, Req2} = cowboy_req:reply(Code, [], Req),
            qdecp_stats:log_event({response, list_to_atom(integer_to_list(Code))}),
            {ok, Req2, State};
        Else -> Else
    end.

terminate(Reason, _Req, _State) ->
    case Reason of
        {normal, shutdown} ->
            lager:debug("Terminating request with ~p", [Reason]);
        _ ->
            lager:info("Terminating request with ~p", [Reason])
    end,
    ok.

clean_request_headers(Req) ->
    {Headers, _} = cowboy_req:headers(Req),
    [{binary_to_list(Name), binary_to_list(Val)} || {Name, Val} <- Headers,
                                                    Name =/= <<"host">>].

clean_response_headers(Headers = [{Name, _} | _]) when is_binary(Name) ->
    Headers;
clean_response_headers(Headers) ->
    lager:debug("Response headers: ~p", [Headers]),
    [{list_to_binary(string:to_lower(Name)), list_to_binary(Val)} ||
        {Name, Val} <- Headers,
        Name =/= "Connect",
        Name =/= "Content-Length",
        Name =/= "Transfer-Encoding"].
