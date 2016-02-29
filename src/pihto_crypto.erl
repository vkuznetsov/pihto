-module(pihto_crypto).
-export([random_hex/0, md5/1]).

md5(Data) ->
    ByteList = erlang:bitstring_to_list(erlang:md5(Data)),
    Fun = fun(N) -> io_lib:format("~2.16.0b", [N]) end,
    erlang:list_to_bitstring(lists:flatten(lists:map(Fun, ByteList))).

random_hex() ->
    md5(crypto:strong_rand_bytes(32)).
