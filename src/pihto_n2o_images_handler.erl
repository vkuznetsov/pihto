-module(pihto_n2o_images_handler).
-export([main/0, event/1, api_event/3]).

-include("include/records.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() ->
    Path = wf:path(?REQ),
    %% main(Path).
    #dtl { file="images",
           app=pihto,
           bindings=[{images, []}, {window_buttons, pihto_n2o_images_view:window_buttons()}] }.

main(<<"/images/tag/", Tag/binary>>) ->
    ok;
main(<<"/images/list/", ListId/binary>>) ->
    io_lib:format("List: ~p", [ListId]);
main(<<"/images", _/binary>>) ->
    io_lib:format("Recent", []).

%% =========================================================================
%% Events
%% =========================================================================

event(init) ->
    {ok, Images} = pihto_image:find_by(1000, #{tag => notag}),
    erlang:put(images, Images),
    Fun = fun(Image) -> wf:insert_bottom(images, pihto_n2o_images_view:image_edit(Image)) end,
    lists:foreach(Fun, Images),
    wf:wire("setup();"),
    wf:wire(#api{name=api_save});
event({select, ImageId}) ->
    Selected = case erlang:get(selected) of undefined -> []; S -> S end,
    case lists:member(ImageId, Selected) of
        true ->
            erlang:put(selected, lists:delete(ImageId, Selected)),
            pihto_n2o_images_view:select(ImageId, false);
        false ->
            erlang:put(selected, [ImageId | Selected]),
            pihto_n2o_images_view:select(ImageId, true)
    end;
event(select_all) ->
    erlang:put(selected, lists:map(fun(Image) -> Image#pihto_image.id end, erlang:get(images))),
    pihto_n2o_images_view:select(all);
event(select_none) ->
    erlang:put(selected, []),
    pihto_n2o_images_view:select(none);
event(edit_list) ->
    ImageIds = erlang:get(selected),
    {ok, ListId} = pihto_list:register(ImageIds),
    wf:redirect(<<"/images/edit?list_id=", ListId/binary>>);
event(_) ->
    [].

api_event(api_save, JSON, _) ->
    ImagesData = jsone:decode(list_to_binary(JSON)),
    wf:info(?MODULE, "JSON: ~p", [ImagesData]),
    wf:wire("api_save_callback(true)").
