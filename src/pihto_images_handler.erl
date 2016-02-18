-module(pihto_images_handler).
-export([init/3, handle/2, terminate/3]).

-define(IMAGES_PER_PAGE, 200).
-include("../include/records.hrl").

init({tcp, http}, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {ok, Images} = pihto_image:find_by(100, #{tag => <<"t1">>}),
    ImagesProps = lists:map(fun(Image) ->
                                    lists:zip(record_info(fields, image), tl(tuple_to_list(Image)))
                            end, Images),
    {ok, Html} = index_view:render([{images, ImagesProps}]),

    {ok, Reply} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Html, Req),
    {ok, Reply, State}.

terminate(_Reason, _Req, _State) ->
    ok.
