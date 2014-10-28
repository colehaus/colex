var choose = function(ev) {
    ev.stopPropagation();
    var el = $(ev.target);
    var ch = $(el.closest('.nest').children()[el.parent().children().index(el)]);
    ch.addClass('open');
    ch.siblings().removeClass('open');
    el.parent().remove();
};

$(function() {
    $('[type="menu"').each(function(_, el_) {
        var position = function(ev) {
            var mn = $(ev.target).closest('[type="menu"]').children('menu').children('.menu');
            mn.offset({top: ev.pageY, left: ev.pageX});
        };
        var addMenuHandlers = function(ev) {
            var mn = $(ev.target).closest('[type="menu"]').children('menu');
            var lis = mn.children('.menu').children('li');
            lis.each(function(_, el) {
                $(el).off().click(choose);
            });
        };

        var el = $(el_);
        el.click(position);
        el.click(addMenuHandlers);
    });
});
