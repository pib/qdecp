-module(qdecp_client_lhttpc).

-behavior(qdecp_client).

-export([init/0, request/6]).


init() ->
    supervisor:start_child(qdecp_sup, {qdecp_client_lhttpc,
                                       {lhttpc_sup, start_link, []},
                                       permanent, 5000, supervisor,
                                       [lhttpc_sup]}),
    ok.


-spec request(inet:ip_address(), qdecp_client:method(), qdecp_client:url(),
              qdecp_client:headers(), qdecp_client:body(), qdecp_client:retries())
             -> {ok, qdecp_client:code(), qdecp_client:headers(), qdecp_client:body()}
                    | {error, qdecp_client:code()}.
request(Ip, Method, Url, Headers, Body, Retries) ->
    Pool = pool_name([Ip], []),
    Opts = [{pool, Pool}, {pool_ensure, true},
            {connect_options, [{ip, Ip}]}
           ],

    case whereis(Pool) of
        undefined ->
            Res = lhttpc:add_pool(Pool),
            lager:info("Started pool ~p with result ~p", [Pool, Res]);
        _ -> ok
    end,
    case lhttpc:request(binary_to_list(Url), Method, Headers, Body, 5000, Opts) of
        {ok, {{Code, _Reason}, ResponseHeaders, ResponseBody}} ->
            {ok, Code, ResponseHeaders, ResponseBody};
        Else ->
            case Retries of
                0 ->
                    {error, 500};
                _ ->
                    lager:error("Error in lhttpc request ~p: ~p, retrying", [Url, Else]),
                    qdecp_stats:log_event({request_retry}),
                    request(Ip, Method, Url, Headers, Body, Retries - 1)
            end
    end.

pool_name([], Acc) ->
    list_to_atom(lists:flatten(string:join(lists:reverse(Acc), "_")));
pool_name([I | Rest], Acc) when is_integer(I) ->
    pool_name(Rest, [integer_to_list(I) | Acc]);
pool_name([I | Rest], Acc) when is_atom(I) ->
    pool_name(Rest, [atom_to_list(I) | Acc]);
pool_name([{A, B, C, D} | Rest], Acc) ->
    pool_name(Rest, [io_lib:format("~.10B.~.10B.~.10B.~.10B", [A, B, C, D]) | Acc]);
pool_name([{A, B, C, D, E, F, G, H} | Rest], Acc) ->
    pool_name(Rest, [io_lib:format("~.16B:~.16B:~.16B:~.16B:~.16B:~.16B:~.16B:~.16B",
                               [A, B, C, D, E, F, G, H]) | Acc]);
pool_name([I | Rest], Acc) when is_list(I) ->
        pool_name(Rest, [I | Acc]).




