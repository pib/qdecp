-module(qdecp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    cadfaerl:start_link(response_cache, 1000),
    Dispatch = cowboy_router:compile([
                %% {Host, list({Path, Handler, Opts})}
                {'_', [{'_', qdecp_cowboy_handler, []}]}
               ]),

    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(proxy_listener, 100,
                      [{port, 8888}],
                      [{env, [{dispatch, Dispatch}]}]),

    qdecp_sup:start_link().

stop(_State) ->
    ok.
