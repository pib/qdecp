-module(qdecp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port  = case application:get_env(qdecp, port) of
                {ok, P} -> P;
                undefined -> 8888
            end,

    HttpConfig = case application:get_env(qdecp, http) of
                     {ok, C} -> C;
                     undefined -> []
                 end,
    HttpClient = proplists:get_value(client, HttpConfig, qdecp_client_lhttpc),

    {ok, Pid} = qdecp_sup:start_link(),
    qdecp_cache:init(),
    HttpClient:init(),

    Dispatch = cowboy_router:compile([
                %% {Host, list({Path, Handler, Opts})}
                {"127.0.0.1", [{"/stats", qdecp_stats_handler, []}]},
                {"localhost", [{"/stats", qdecp_stats_handler, []}]},
                {'_', [{'_', qdecp_cowboy_handler, [HttpConfig]}]}
               ]),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(proxy_listener, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]),
    {ok, Pid}.


stop(_State) ->
    ok.
