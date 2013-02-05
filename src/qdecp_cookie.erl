-module(qdecp_cookie).

-export([parse_cookies/1, parse_sub_cookies/2]).

parse_cookies(Cookies) when is_binary(Cookies) ->
    parse_cookies(binary_to_list(Cookies));
parse_cookies(Cookies) ->
    parse_cookies(Cookies, []).

parse_cookies([], []) ->
    [];
parse_cookies([], [{Key} | Acc]) ->
    [{lists:reverse(Key), ""} | Acc];
parse_cookies([], [{Key, Val} | Acc]) ->
    [{Key, lists:reverse(Val)} | Acc];
parse_cookies(Cookies, []) ->
    parse_cookies(Cookies, [{""}]);
parse_cookies("; " ++ Cookies, [{Key} | Acc]) ->
    parse_cookies(Cookies, [{lists:reverse(Key), ""} | Acc]);
parse_cookies("; " ++ Cookies, [{Key, Val} | Acc]) ->
    parse_cookies(Cookies, [{""}, {Key, lists:reverse(Val)} | Acc]);
parse_cookies("=" ++ Cookies, [{Key} | Acc]) ->
    parse_cookies(Cookies, [{lists:reverse(Key), ""} | Acc]);
parse_cookies([Char | Cookies], [{Key} | Acc]) ->
    parse_cookies(Cookies, [{[Char | Key]} | Acc]);
parse_cookies([Char | Cookies], [{Key, Val} | Acc]) ->
    parse_cookies(Cookies, [{Key, [Char | Val]} | Acc]).

parse_sub_cookies(Cookie, Div) ->
    parse_sub_cookies(Cookie, Div, []).

parse_sub_cookies([], _, []) ->
    [];
parse_sub_cookies([], _, [{Key} | Acc]) ->
    [{lists:reverse(Key), ""} | Acc];
parse_sub_cookies([], _, [{Key, Val} | Acc]) ->
    [{Key, lists:reverse(Val)} | Acc];
parse_sub_cookies(Cookie, Div, []) ->
    parse_sub_cookies(Cookie, Div, [{""}]);
parse_sub_cookies([Div | Cookie], Div, [{Key} | Acc]) ->
    parse_sub_cookies(Cookie, Div, [{""}, {lists:reverse(Key), ""} | Acc]);
parse_sub_cookies("=" ++ Cookie, Div, [{Key} | Acc]) ->
    parse_sub_cookies(Cookie, Div, [{lists:reverse(Key), ""} | Acc]);
parse_sub_cookies([Char | Cookie], Div, [{Key} | Acc]) ->
    parse_sub_cookies(Cookie, Div, [{[Char | Key]} | Acc]);
parse_sub_cookies([Div | Cookie], Div, [{Key, Val} | Acc]) ->
    parse_sub_cookies(Cookie, Div, [{""}, {Key, lists:reverse(Val)} | Acc]);
parse_sub_cookies([Char | Cookie], Div, [{Key, Val} | Acc]) ->
    parse_sub_cookies(Cookie, Div, [{Key, [Char | Val]} | Acc]).



