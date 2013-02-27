
-module(qdecp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, mnesia_start_link/0, mnesia_init/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
           [
            {mnesia_sup, {qdecp_sup, mnesia_start_link, []}, permanent, 5000, supervisor, [mnesia_sup]},
            ?CHILD(qdecp_stats, worker)
           ]}}.

mnesia_start_link() ->
    proc_lib:start_link(qdecp_sup, mnesia_init, [self()]).

mnesia_init(Parent) ->
    case mnesia_sup:start() of
        {ok, Pid} ->
            proc_lib:init_ack(Parent, {ok, Pid});
        {error, {already_started, Pid}} ->
            proc_lib:init_ack(Parent, {ok, Pid})
    end.
    
    
