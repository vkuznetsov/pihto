-module(pihto_image).
-export([create/1, find/1, find/2, find_by/2]).

-include("../include/records.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/metainfo.hrl").

-spec create(#pihto_image{}) -> {ok, any()} | {error, any()}.
create(Image) ->
    NormalizedImage = normalize_image(Image),
    ImageTagsNames = normalize_tags(NormalizedImage#pihto_image.tags),
    ImageId = NormalizedImage#pihto_image.id,
    UserId = NormalizedImage#pihto_image.user_id,

    CreateFn = fun() ->
                       mnesia:write(NormalizedImage),
                       lists:foreach(fun(TagName) ->
                                             UserTag = #pihto_user_tag{
                                                          user_id = UserId,
                                                          image_id = ImageId,
                                                          user_and_tag = {UserId, TagName}
                                                         },
                                             mnesia:write(UserTag)
                                     end,
                                     ImageTagsNames)
               end,
    case mnesia:transaction(CreateFn) of
        {atomic, _} ->
            spawn(pihto_thumb, create, [NormalizedImage]),
            {ok, NormalizedImage};
        {aborted, Reason} ->
            {error, Reason}
    end.

-spec find(id()) -> {ok, #pihto_image{}} | notfound.
find(ImageId) ->
    case mnesia:dirty_read({pihto_image, ImageId}) of
        [] -> notfound;
        [Image] -> {ok, Image}
    end.

-spec find(id(), id()) -> {ok, #pihto_image{}} | notfound.
find(UserId, ImageId) ->
    case mnesia:dirty_read({pihto_image, ImageId}) of
        [Image] when Image#pihto_image.user_id == UserId  -> {ok, Image};
        _ -> notfound
    end.

-spec find_by(id(), #{}) -> {ok, [#pihto_image{}]}.
find_by(UserId, #{tag := TagName}) ->
    Tags = mnesia:dirty_index_read(pihto_user_tag, {UserId, TagName}, #pihto_user_tag.user_and_tag),
    Images = lists:map(fun(#pihto_user_tag{image_id = ImageId}) ->
                               {ok, Image} = find(UserId, ImageId),
                               Image
                       end, Tags),
    {ok, Images}.

%% =============================================================================
%% Internal functions
%% =============================================================================

normalize_image(Image) ->
    ImageId = case Image#pihto_image.id of undefined -> {ok, Id} = next_id(), Id; Id -> Id end,
    CreatedAt = case Image#pihto_image.created_at of undefined -> erlang:timestamp(); T -> T end,
    Image#pihto_image{id = ImageId,
                created_at = CreatedAt
               }.

normalize_tags(undefined) -> normalize_tags([]);
normalize_tags([]) -> [notag];
normalize_tags(ImageTagsNames) -> ImageTagsNames.

next_id() ->
    Fun = fun() ->
                  Value = case mnesia:read({pihto_sequence, image}) of
                              [] -> 1;
                              [#pihto_sequence{value = V, name = image}] -> V + 1
                          end,

                  mnesia:write(#pihto_sequence{value = Value, name = image}),
                  Value
          end,
    case mnesia:transaction(Fun) of
        {atomic, Value} -> {ok, Value};
        {aborted, Reason} -> {error, Reason}
    end.
