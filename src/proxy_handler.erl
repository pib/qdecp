-module(proxy_handler).
-behavior(cowboy_loop_handler).
-export([init/3, info/3, terminate/3]).

-record(state, {req_id, res_code, res_headers, res_body=[]}).

init({tcp, http}, Req, _) ->
    case proxy_server:proxy(Req, self()) of
        {ok, ReqId} ->
            {loop, Req, #state{req_id=ReqId}, 5000, hibernate};
        {not_allowed, Allowed} ->
            {ok, Req2} = cowboy_req:reply(405, [{<<"allowed">>, Allowed}], Req),
            {ok, Req2, undefined_state}
    end.

info({proxy_headers, ReqId, Code, Headers}, Req, State) ->
    case State#state.req_id of
        ReqId ->
            %% The request id matches, so we're good
            {loop, Req, State#state{res_code=Code, res_headers=Headers}, hibernate};
        _ ->
            %% Request id didn't match, ignore it
            {loop, Req, State, hibernate}
    end;
info({proxy_body, ReqId, Body}, Req, State) ->
    case State#state.req_id of
        ReqId ->
            %% The request id matches, so we're good
            {loop, Req, State#state{res_body=[Body | State#state.res_body]}, hibernate};
        _ ->
            %% Request id didn't match, ignore it
            {loop, Req, State, hibernate}
    end;
info({proxy_done, ReqId}, Req, State) ->
    case State#state.req_id of
        ReqId ->
            %% The request id matches, so we're good
            Body = lists:reverse(State#state.res_body),
            Code = State#state.res_code,
            Headers = lists:reverse(State#state.res_headers),
            {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
            {ok, Req2, State};
        _ ->
            %% Request id didn't match, ignore it
            {loop, Req, State, hibernate}
    end;

info(Message, Req, State) ->
    lager:warning("Ignoring unrecognized message ~p", [Message]),
    {loop, Req, State, hibernate}.


terminate(Reason, _Req, State) ->
    lager:info("Terminating request ~p with ~p", [State#state.req_id, Reason]),
    ok.

