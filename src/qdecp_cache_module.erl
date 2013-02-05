-module(qdecp_cache_module).

-callback init([{atom(), term()}]) ->
    ok | {error, any()}.

-callback set(iolist(), iolist()) ->
    ok | {error, any()}.

-callback get(iolist()) ->
    {ok, iolist()} | none.

