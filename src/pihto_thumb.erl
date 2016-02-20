-module(pihto_thumb).
-export([create/1, path/2]).

-include("include/records.hrl").

-define(PATH, "priv/static/thumbs").
-define(COMMAND, "bin/thumb").

-spec create(#pihto_image{}) -> {ok, digest()}.
create(Image) ->
    URL = Image#pihto_image.url,
    UrlDigest = digest(URL),
    Referer = Image#pihto_image.origin,

    ThumbDigest = case read(UrlDigest) of
                      {ok, TD} -> TD;
                      notfound ->
                          {ok, TD} = download_and_convert(URL, Referer),
                          ok = write(UrlDigest, TD),
                          TD
                  end,
    ok = update_image(Image#pihto_image{thumb_digest=ThumbDigest}).

path(big, Image) ->
    case Image#pihto_image.thumb_digest of
        undefined -> Image#pihto_image.url;
        ThumbDigest -> <<"/static/thumbs", ThumbDigest/binary, ".big.jpg">>
    end;
path(small, Image) ->
    case Image#pihto_image.thumb_digest of
        undefined -> <<"/static/thumbs/default.thumb.jpg">>;
        ThumbDigest -> <<"/static/thumbs/", ThumbDigest/binary, ".thumb.jpg">>
    end.

%% =========================================================================
%% Internal functions
%% =========================================================================

-spec read(digest()) -> notfound | {ok, digest()}.
read(UrlDigest) ->
    case mnesia:dirty_read({pihto_url_digest, UrlDigest}) of
        [] -> notfound;
        [Record] -> {ok, Record#pihto_url_digest.thumb_digest}
    end.

write(UrlDigest, ThumbDigest) ->
    mnesia:transaction(fun() ->
                               ok = mnesia:write(#pihto_url_digest{digest=UrlDigest, thumb_digest=ThumbDigest})
                       end),
    ok.

update_image(Image) ->
    mnesia:transaction(fun() ->
                               ok = mnesia:write(Image)
                       end),
    ok.

download_and_convert(URL, Referer) ->
    case sh:run([?COMMAND, URL, Referer, path()]) of
        {done, 0, ThumbDigest} -> {ok, ThumbDigest};
        {done, Status, Message} ->
            io:format("Can't download and create image for ~p~nError: ~p ~p~n", [URL, Status, Message]),
            {error, Status, Message}
    end.

path() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join(Cwd, ?PATH).

digest(Val) ->
    SHA = erlsha2:sha224(Val),
    hmac:hexlify(SHA, [lower, binary]).
