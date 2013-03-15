-module(qdecp_client).

-type method() :: get | post.
-type url() :: binary().
-type headers() :: [{binary(), iodata()}].
-type body() :: iodata().
-type retries() :: integer().
-type code() :: integer().

-callback init() ->
    ok.

-callback request(inet:ip_address(), method(), url(), headers(), body(), retries())
	-> {ok, code(), headers(), body()} | {error, code()}.

