-module(qdecp_connect_proxy).

%% API.
-export([upgrade/4]).

-record(state, {
          req :: cowboy_req:req(),
          env :: cowboy_middleware:env(),
          in_socket = undefined :: inet:socket(),
          out_socket = undefined :: inet:socket(),
          transport = undefined :: module(),
          messages = undefined :: {atom(), atom(), atom()}
}).

%% @doc Upgrade an HTTP request to a pass-through transport
-spec upgrade(Req, Env, module(), any())
	-> {ok, Req, Env} | {error, 400, Req}
	| {suspend, module(), atom(), [any()]}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
upgrade(Req, Env, _Handler, _HandlerOpts) ->
	{_, ListenerPid} = lists:keyfind(listener, 1, Env),
	ranch_listener:remove_connection(ListenerPid),

    lager:debug("Upgrading request via CONNECT ~p", [Req]),

    case init_proxy(Req, Env) of
        {ok, State} ->
            lager:debug("Connected, entering loop ~p", [State]),
            loop(State);
        {error, Reason, Req2} ->
            lager:error("Connect failed: ~p, ~p", [Reason, Req2]),
            {error, 400, Req2}
    end.

init_proxy(Req, Env) ->
	[Host, Port, InSock, Transport] = cowboy_req:get([host, port, socket, transport], Req),
    Messages = Transport:messages(),
    {ok, {Ip, _Port}} = inet:sockname(InSock),
    OutSockOpts = [{mode, binary},
                   {active, false},
                   {ip, Ip},
                   {linger, {true, 5}}
                  ],
    Transport:setopts(InSock, [{linger, {true, 5}}]),
    lager:debug("Connecting to ~p ~p with opts ~p", [Host, Port, OutSockOpts]),
    case gen_tcp:connect(binary_to_list(Host), Port, OutSockOpts) of
        {ok, OutSock} ->
            lager:debug("Opened outgoing socket ~p ~p", [Host, Port]),
            qdecp_stats:log_event({request_out, connect}),            
            State = #state{env=Env, in_socket=InSock, out_socket=OutSock,
                           transport=Transport, messages=Messages, req=Req},
            Transport:send(InSock, [<<"HTTP/1.0 200 Connection established\r\n">>,
                                    <<"Proxy-agent: qdecp/1\r\n\r\n">>]),
            Transport:setopts(InSock, [{active, once}]),
            inet:setopts(OutSock, [{active, once}]),
            {ok, State};
        {error, Reason} ->
            lager:error("Failed to open outgoing socket ~p ~p ~p", [Host, Port, Reason]),
            qdecp_stats:log_event({connect_connect_error, Reason}),
            {error, Reason}
    end.
    

loop(State) ->
    #state{in_socket = InSock,
           out_socket = OutSock,
           transport = Transport,
           messages = {OK, Closed, Error},
           req = Req,
           env = Env
          } = State,
    receive
        {OK, InSock, Data} ->
            gen_tcp:send(OutSock, Data),
            Transport:setopts(InSock, [{active, once}]),
            loop(State);
        {Closed, InSock} ->
            lager:debug("Incoming socket closed ~p", [InSock]),
            gen_tcp:close(OutSock),
            qdecp_stats:log_event({connect_close, insock}),
            {ok, Req, [{result, closed} | Env]};
        {Error, InSock, Reason} ->
            lager:error("Error on incoming socket ~p: ~p", [InSock, Reason]),
            qdecp_stats:log_event({connect_socket_error, Reason, insock}),
            {error, 400, Req};
        {tcp, OutSock, Data} ->
            Transport:send(InSock, Data),
            inet:setopts(OutSock, [{active, once}]),
            loop(State);
        {tcp_closed, OutSock} ->
            lager:debug("Outgoing socket closed ~p", [OutSock]),
            Transport:close(InSock),
            qdecp_stats:log_event({connect_close, outsock}),
            {ok, Req, [{result, closed} | Env]};
        {tcp_error, OutSock, Reason} ->
            lager:error("Error on outgoing socket ~p: ~p", [OutSock, Reason]),
            qdecp_stats:log_event({connect_socket_error, Reason, outsock}),
            {error, 400, Req}
    end.
            
            
