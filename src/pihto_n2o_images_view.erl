-module(pihto_n2o_images_view).

-export([image/1, image_edit/1, window_buttons/0]).
-export([select/1, select/2]).

-include("include/records.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

window_buttons() ->
    [
     #button {type="button", id="btn-select-all", class=["btn", "btn-sm", "btn-default"], postback=select_all, body=
                  [#span {class=["glyphicon", "glyphicon-ok"], aria_states=[{"aria-hidden", "true"}]},
                   "Select All"
                  ]},
     #button {type="button", id="btn-select-none", class=["btn", "btn-sm", "btn-default"], postback=select_none, body=
                  [#span {class=["glyphicon", "glyphicon-remove"], aria_states=[{"aria-hidden", "true"}]},
                   "Select None"
                  ]},
     #button {type="button", id="btn-edit", class=["btn", "btn-sm", "btn-primary"], postback=edit_list, body=
                  [#span {class=["glyphicon", "glyphicon-edit"], aria_states=[{"aria-hidden", "true"}]},
                   "Edit"
                  ]},
     #button {type="button", id="btn-delete", class=["btn", "btn-sm", "btn-danger"], postback=delete_list, body=
                  [#span {class=["glyphicon", "glyphicon-trash"], aria_states=[{"aria-hidden", "true"}]},
                   "Delete"
                  ]}].

image(Image) ->
    #panel {class=["col-lg-2 col-md-3 col-sm-4 col-xs-6"], body=image_thumb(Image)}.

image_edit(Image) ->
    #panel {class=["col-lg-6 col-md-6 col-sm-12 col-xs-12 multiple-editor"],
            body=[#form {
                     body=[#panel {class=["panel"],
                                   body=[#table {
                                            body=[#tr {
                                                     cells=[#td {body=image_thumb(Image)},
                                                            #td {body=image_fields(Image)}]}]}]}]}]}.

image_thumb(Image) ->
    #panel {id="image_" ++ integer_to_list(Image#pihto_image.id), class=["thumb-box thumbnail"],
            body=[#span {class=["thumb-box-overlay"],
                         body=[#span {class=["thumb-box-overlay-background"]},
                               image_buttons(Image)]},
                  #link {href=pihto_thumb:path(big, Image), target="_blank",
                         body=[#image {src=pihto_thumb:path(small, Image), title=Image#pihto_image.description}] }]}.

image_buttons(Image) ->
    ImageId = Image#pihto_image.id,
    [
     #link {title="Select", onclick=io_lib:format("select(~p)", [ImageId]), postback={select, ImageId},
            body=#span{class=["glyphicon glyphicon-ok selector"]}},
     #link {title="Go to origin", href=Image#pihto_image.origin, target="_blank",
            body=#span{class=["glyphicon glyphicon-share-alt"]}},
     #link {title="Edit", postback={edit, ImageId},
            body=#span{class=["glyphicon glyphicon-edit"]}},
     #link {title="Delete", postback={delete, ImageId},
            body=#span{class=["glyphicon glyphicon-trash"]}}
    ].

image_fields(Image) ->
    ImageId = Image#pihto_image.id,
    [#panel {class=["form-group"],
             body=#textarea {class=["form-control"], name=io_lib:format("image[~p][description]", [ImageId]), placeholder="description",
                             body=Image#pihto_image.description}},
     #panel {class=["form-group"],
             body=#input {class=["form-control"], name=io_lib:format("image[~p][tags]", [ImageId]), type="text", placeholder="tags"}}].

%% =========================================================================
%% Actions
%% =========================================================================

select(ImageId, false) ->
    wf:wire(io_lib:format("$('#image_~p').removeClass('selected')", [ImageId]));
select(ImageId, true) ->
    wf:wire(io_lib:format("$('#image_~p').addClass('selected');", [ImageId])).

select(all) ->
    wf:wire("$('.thumb-box').addClass('selected');");
select(none) ->
    wf:wire("$('.thumb-box').removeClass('selected');").
