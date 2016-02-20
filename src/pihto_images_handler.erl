-module(pihto_images_handler).

-export([
         init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         delete_resource/2,
         get_resource/2,
         put_resource/2
]).

-define(IMAGES_PER_PAGE, 200).

-include("include/records.hrl").

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {
        [{{<<"text">>, <<"html">>, []}, get_resource}],
        Req, State
    }.

content_types_accepted(Req, State) ->
    {
        [{{<<"application">>, <<"json">>, '*'}, put_resource}],
        Req, State
    }.

delete_resource(Req, State) ->
    {true, Req, State}.

get_resource(Req, State) ->
    case cowboy_req:binding(id, Req) of
        {undefined, Req1} ->
            get_images(Req1, State);
        {_ImageId, Req1} ->
            {ok, Reply} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"Not found">>, Req1),
            {ok, Reply, State}
    end.

put_resource(Req, State) ->
    {ok, ImageJSON, Req1} = cowboy_req:body(Req),
    {ImageData} = jsone:decode(ImageJSON),
    Image = #pihto_image{
               user_id = 1000,
               tags = proplists:get_value(<<"tags">>, ImageData),
               url = proplists:get_value(<<"url">>, ImageData),
               origin = proplists:get_value(<<"origin">>, ImageData),
               description = proplists:get_value(<<"description">>, ImageData)
              },
    case pihto_image:create(Image) of
        {ok, _} ->
            wf:info(?MODULE, "Image saved: ~p", [Image]),
            {true, Req1, State};
        {error, Reason} ->
            wf:error(?MODULE, "Can't save image~nImage: ~p~nReason: ~p", [Image, Reason]),
            {ok, Reply} = cowboy_req:reply(404, Req1),
            {halt, Reply, State}
    end.


get_images(Req, State) ->
    %% {TagName, Req2} = cowboy_req:qs_val(<<"tag">>, Req1),
    {ok, Images} = pihto_image:find_by(1000, #{tag => notag}),
    ImagesProps = lists:map(fun(Image) ->
                                    lists:zip(record_info(fields, pihto_image), tl(tuple_to_list(Image)))
                            end, Images),
    {ok, Html} = index_view:render([{images, ImagesProps}]),

    %% {ok, Reply} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/html">>}], Html, Req),
    {Html, Req, State}.
