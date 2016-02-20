-ifndef(PIHTO_IMAGE_HRL).
-define(PIHTO_IMAGE_HRL, true).

-include("types.hrl").
-include_lib("kvs/include/kvs.hrl").

-record(pihto_sequence, {name :: term(),
                   value :: integer()
                  }).

-record(pihto_image, {id :: id(),
                user_id :: id(),
                url :: url(),
                thumb_digest :: digest(),
                tags :: [string()],
                origin :: url(),
                description :: string(),
                created_at :: erlang:timestamp()
               }).

-record(pihto_user_tag, {user_id :: id(),
                   image_id :: id(),
                   user_and_tag :: {id(), string()}
                  }).

-record(pihto_url_digest, {digest :: digest(),
                     thumb_digest :: digest()
                    }).

-endif.
