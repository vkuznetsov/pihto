-module(pihto_list).
-export([register/1, get/1]).

-include("include/records.hrl").

-spec register([id()]) -> {ok, string()} | {error, any()}.
register(Items) when is_list(Items) ->
    ListId = pihto_crypto:random_hex(),
    Fun = fun() -> ok = mnesia:write(#pihto_list{id=ListId, items=Items}) end,
    case mnesia:transaction(Fun) of
        {atomic, _} -> {ok, ListId};
        {aborted, Reason} -> {error, Reason}
    end.

-spec get(string()) -> {ok, [id()]} | {error, any()}.
get(ListId) ->
    Fun = fun() -> mnesia:read({pihto_list, ListId}) end,
    case mnesia:transaction(Fun) of
        {atomic, []} -> {ok, []};
        {atomic, [#pihto_list{items=Items}]} -> {ok, Items};
        {aborted, Reason} -> {error, Reason}
    end.
