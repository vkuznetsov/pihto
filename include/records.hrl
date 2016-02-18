-ifndef(IMAGE_HRL).
-define(IMAGE_HRL, true).

-include("types.hrl").
-include_lib("kvs/include/kvs.hrl").

-record(sequence, {name :: term(),
                   value :: integer()
                  }).

-record(image, {id :: id(),
                user_id :: id(),
                url :: url(),
                url_digest :: digest(),
                tags :: [string()],
                origin :: url(),
                description :: string(),
                created_at :: erlang:timestamp()
               }).

-record(tag, {user_and_tag :: {integer(), string()},
              user_and_image :: {integer(), digest()}
             }).

-endif.
