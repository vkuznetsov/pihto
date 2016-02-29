var changes, timer;

function select(image_id) {
    $('#image_' + image_id).toggleClass('selected');
}

function setup() {
    changes = new Object();

    $('.multiple-editor .form-control').change(function() {
        if (timer) clearTimeout(timer);

        var image_id, attr_name;
        [_, image_id, attr_name] = $(this).attr("name").match(/\[(\d+)\]\[(.+)\]/); // image[2][description]

        if (!changes[image_id]) changes[image_id] = new Object();
        changes[image_id][attr_name] = $(this).val();

        timer = setTimeout(save_changes, 5000);
    });

    $(window).unload(save_changes);
}

function save_changes() {
    api_save(changes);
}

function api_save_callback(success) {
    console.log(JSON.stringify(changes));

    if (success) {
        changes = new Object();
    }
    else {
        timer = setTimeout(save_changes, 5000);
    }
}
