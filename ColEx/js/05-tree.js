$(function() {

var choose = function(ev) {
    ev.stopPropagation();
    var el = $(ev.target);
    var ch = $(el.closest('.nest').children()[el.index()]);
    ch.addClass('open');
    ch.siblings().removeClass('open');
    defaultHandlers(el);
};

$('[type="menu"').each(function(_, el_) {
    var position = function(ev) {
        var mn = $(ev.target).closest('[type="menu"]').children('menu').children('.menu');
        mn.offset({top: ev.pageY, left: ev.pageX});
    };
    var addMenuHandlers = function(ev) {
        $(ev.target).closest('[type="menu"]').children('menu').
            children('.menu').children('li').off().click(choose);
    };

    var el = $(el_);
    el.click(position);
    el.click(addMenuHandlers);
});

});
