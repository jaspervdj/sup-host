function findHostElement(host) {
    return $('li[data-host="' + host + '"]').children('.host-content');
}

function showHost(host) {
    $.get('/' + host, function(data) {
        findHostElement(host).html(data);
    });
}

function wakeHost(host) {
    $.post('/' + host + '/wake', function(data) {
        findHostElement(host).html(data);
    });

    return false;
}
