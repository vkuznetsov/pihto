-module(pihto_schema).

-compile(export_all).

-define(NODES, [node() | nodes()]).

-include("include/records.hrl").

create_schema() ->
    application:stop(mnesia),
    mnesia:create_schema(?NODES),
    ok = application:start(mnesia).

create_sequence() ->
    mnesia:create_table(pihto_sequence, [{attributes, record_info(fields, pihto_sequence)},
                                   {disc_copies, ?NODES},
                                   {type, set}
                                  ]).

create_images() ->
    mnesia:create_table(pihto_image, [{attributes, record_info(fields, pihto_image)},
                                {index, [user_id]},
                                {disc_copies, ?NODES},
                                {type, set}
                               ]).

create_user_tags() ->
    mnesia:create_table(pihto_user_tag, [{attributes, record_info(fields, pihto_user_tag)},
                                   {index, [user_and_tag]},
                                   {disc_copies, ?NODES},
                                   {type, bag}
                                  ]).

create_url_digests() ->
    mnesia:create_table(pihto_url_digest, [{attributes, record_info(fields, pihto_url_digest)},
                                     {disc_copies, ?NODES},
                                     {type, set}
                                    ]).

create_tables() ->
    {atomic, ok} = create_sequence(),
    {atomic, ok} = create_images(),
    {atomic, ok} = create_user_tags(),
    {atomic, ok} = create_url_digests().
