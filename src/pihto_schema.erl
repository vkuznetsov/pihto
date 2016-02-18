-module(pihto_schema).

-compile(export_all).

-define(NODES, [node() | nodes()]).

-include("records.hrl").

create_schema() ->
    application:stop(mnesia),
    mnesia:create_schema(?NODES),
    ok = application:start(mnesia).

create_sequence() ->
    mnesia:create_table(sequence, [{attributes, record_info(fields, sequence)},
                                   {disc_copies, ?NODES},
                                   {type, set}]).

create_images() ->
    mnesia:create_table(image, [{attributes, record_info(fields, image)},
                                {index, [user_id]},
                                {disc_copies, ?NODES},
                                {type, set}]).

create_tags() ->
    mnesia:create_table(tag, [{attributes, record_info(fields, tag)},
                              {index, [user_and_image]},
                              {disc_copies, ?NODES},
                              {type, bag}]).

create() ->
    create_schema(),
    create_sequence(),
    create_images(),
    create_tags().
