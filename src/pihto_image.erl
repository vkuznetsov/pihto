-module(pihto_image).
-export([create/1, find/1, find/2, find_by/2]).

-include("../include/records.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/metainfo.hrl").

-spec create(#image{}) -> {ok, any()} | {error, any()}.
create(Image) ->
    UserId = Image#image.user_id,
    {ok, ImageId} = next_id(),
    ImageTags = Image#image.tags,
    ImageWithId = Image#image{id = ImageId},
    CreateFn = fun() ->
                       mnesia:write(ImageWithId),
                       lists:foreach(fun(ImageTag) ->
                                             Tag = #tag{user_and_tag={UserId, ImageTag}, user_and_image={UserId, ImageId}},
                                             mnesia:write(Tag)
                                     end,
                                     ImageTags)
               end,
    case mnesia:transaction(CreateFn) of
        {atomic, _} -> {ok, ImageWithId};
        {aborted, Reason} -> {error, Reason}
    end.

-spec find(id()) -> {ok, #image{}} | notfound.
find(ImageId) ->
    case mnesia:dirty_read({image, ImageId}) of
        [] -> notfound;
        [Image] -> {ok, Image}
    end.

-spec find(id(), id()) -> {ok, #image{}} | notfound.
find(UserId, ImageId) ->
    case mnesia:dirty_read({image, ImageId}) of
        [Image] when Image#image.user_id == UserId  -> {ok, Image};
        _ -> notfound
    end.

-spec find_by(id(), #{}) -> {ok, [#image{}]}.
find_by(UserId, #{tag := Tag}) ->
    Tags = mnesia:dirty_read({tag, {UserId, Tag}}),
    Images = lists:map(fun(#tag{user_and_image = {_, ImageId}}) ->
                               {ok, Image} = find(UserId, ImageId),
                               Image
                       end, Tags),
    {ok, Images}.

%% =============================================================================
%% Internal functions
%% =============================================================================

next_id() ->
    Fun = fun() ->
                  Value = case mnesia:read({sequence, image}) of
                              [] -> 0;
                              [#sequence{value = V, name = image}] -> V + 1
                          end,
                  mnesia:write(#sequence{value = Value, name = image}),
                  Value
          end,
    case mnesia:transaction(Fun) of
        {atomic, Value} -> {ok, Value};
        {aborted, Reason} -> {error, Reason}
    end.
