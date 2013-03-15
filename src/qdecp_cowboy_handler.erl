-module(qdecp_cowboy_handler).
-behavior(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-record(state, {http_client}).

init({tcp, http}, Req, [HttpClient]) ->
    {ok, Req, #state{http_client=HttpClient}}.

handle(Req, State) ->
    {Method, _} = cowboy_req:method(Req),
    qdecp_stats:log_event({request_in, list_to_atom(string:to_lower(binary_to_list(Method)))}),
    case qdecp_cache:get(Req) of
        {ok, {Code, Headers, Body}} ->
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            {ok, Req2, State};
        none ->
            do_request(Req, State)
    end.

do_request(Req, State=#state{http_client=HttpClient}) ->
    {ReqMethod, _} = cowboy_req:method(Req),
    {Url, _} = cowboy_req:url(Req),
    ReqHeaders = clean_request_headers(Req),
    [Socket] = cowboy_req:get([socket], Req),
    {ok, {Ip, _Port}} = inet:sockname(Socket),
    lager:debug("~p ~p ~p", [Ip, ReqMethod, Url]),
    Reply = case ReqMethod of
                <<"GET">> ->
                    HttpClient:request(Ip, get, Url, ReqHeaders, [], 10);
                <<"POST">> ->
                    {ok, ReqBody, _} = cowboy_req:body(Req),
                    HttpClient:request(Ip, post, Url, ReqHeaders, ReqBody, 10);
                <<"CONNECT">> ->
                    {upgrade, protocol, qdecp_connect_proxy};
                _ ->
                    {ok, "405", [{<<"allow">>, <<"GET, POST, CONNECT">>}], []}
            end,

    case Reply of
        {ok, Code, Headers, Body} ->
            CleanHeaders = clean_response_headers(Headers),
            qdecp_cache:set(Req, {Code, Headers, Body}),
            {ok, Req2} = cowboy_req:reply(Code, CleanHeaders, Body, Req),
            qdecp_stats:log_event({response, list_to_atom(integer_to_list(Code))}),
            {ok, Req2, State};
        {error, Code} ->
            {ok, Req2} = cowboy_req:reply(Code, [], Req),
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
