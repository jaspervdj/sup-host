function findHostElement(host) {
    return $("li:contains('" + host + "')");
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
