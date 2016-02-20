-module(pihto_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).
-export([log_request/1, log_response/4]).

%% =============================================================================
%% Application callbacks
%% =============================================================================

start() ->
    application:ensure_all_started(pihto),
    application:set_env(n2o, route, pihto_n2o_routes),
    application:start(pihto).

start(_StartType, _StartArgs) ->
    random:seed(erlang:timestamp()),

    Dispatch = cowboy_router:compile(cowboy_routes()),
    {ok, _} = cowboy:start_http(my_http_listener, 10, [{port, 8080}],
                                [
                                 {env, [{dispatch, Dispatch}]},
                                 {onrequest, fun ?MODULE:log_request/1},
                                 {onresponse, fun ?MODULE:log_response/4}
                                ]
                               ),

    pihto_sup:start_link().

stop(_State) ->
    ok.

log_request(Req) ->
    {RequestId, Req1} = case cowboy_req:header(<<"x-request-id">>, Req) of
                            {undefined, R} -> {pihto_md5:md5_hex(crypto:strong_rand_bytes(32)), R};
                            {Val, R} -> {Val, R}
                        end,

    Req2 = cowboy_req:set_resp_header(<<"x-request-id">>, RequestId, Req1),

    {Method, Req3} = cowboy_req:method(Req2),
    {URL, Req4} = cowboy_req:url(Req3),
    {ok, Body, _} = cowboy_req:body_qs(Req4),
    io:format("REQUEST [~s] ~s ~s ~p~n", [RequestId, Method, URL, Body]),
    Req4.

log_response(Status, Headers, _Body, Req) ->
    io:format("RESPONSE [~s] Status: ~p~n", [proplists:get_value(<<"x-request-id">>, Headers), Status]),
    Req.

%% =============================================================================
%% Routes
%% =============================================================================
cowboy_routes() ->
    [{'_', [{"/static/[...]", n2o_static, { dir, "priv/static", mime() }},
            {"/n2o/[...]", n2o_static, { dir, "deps/n2o/priv", mime() }},
            {"/images/[:id]", pihto_images_handler, []},
            { "/ws/[...]", n2o_stream, []},
            {'_', n2o_cowboy, []}
           ]}].

%% =============================================================================
%% Internal functions
%% =============================================================================
mime() -> [ { mimetypes, cow_mimetypes, all } ].
