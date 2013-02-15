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

    {ok, Pid} = qdecp_sup:start_link(),
    qdecp_cache:init(),

    Dispatch = cowboy_router:compile([
                %% {Host, list({Path, Handler, Opts})}
                {'_', [{'_', qdecp_cowboy_handler, []}]}
               ]),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(proxy_listener, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]),
    {ok, Pid}.


stop(_State) ->
    ok.
