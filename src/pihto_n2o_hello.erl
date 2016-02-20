-module(pihto_n2o_hello).
-export([main/0, event/1, loop/1]).

-include("../include/records.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").


main() ->
    #dtl { file="images",
           app=pihto,
           bindings=[{images, images()}] }.


images() ->
    {ok, Images} = pihto_image:find_by(1000, #{tag => notag}),

    ImageBlock = fun(Image) ->
                         #panel {class=["col-lg-2 col-md-3 col-sm-4 col-xs-6"],
                                 body=[#panel {class=["thumb-box", "thumbnail"],
                                               data_fields=[{'data-image-id', Image#pihto_image.id}],
                                               body=image_thumbnail(Image)
                                              }]}
                 end,

    lists:map(ImageBlock, Images).

image_thumbnail(Image) ->
    [#span {class=["thumb-box-overlay"], body=image_buttons(Image)},
     #link {href=pihto_thumb:path(big, Image),
            target="_blank",
            body=[#image {src=pihto_thumb:path(small, Image), title=Image#pihto_image.description}]
           }].

image_buttons(Image) ->
    [#span {class=["thumb-box-overlay-background"]},
     #span {class=["glyphicon", "glyphicon-ok"], title="Select", postback=select},
     #span {class=["glyphicon", "glyphicon-star"], title="Add to favorites"},
     #link {class=["glyphicon", "glyphicon-share-alt"], title="Go to origin", href=Image#pihto_image.origin},
     #link {class=["glyphicon", "glyphicon-edit"], title="Edit", href="#"},
     #link {class=["glyphicon", "glyphicon-trash"], title="Delete", href="#"}
    ].
event(init) ->
    wf:reg(room),
    wf:async("looper",fun loop/1);
event(select) ->
    wf:info(?MODULE, "Hello~n", []).

loop({room,User,Message}) ->
    wf:flush(room).
