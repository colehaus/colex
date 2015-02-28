// Use menus to build tree of choices
(function($, menu, sidenote) {
$(function() {
'use strict';

var transition = function(from, to, finalCb) {
    // If select active option, do nothing
    if(typeof from[0] === 'undefined') { return; }
    var parent = to.parent();
    if (!parent.is(':visible')) { from.removeClass('open'); to.addClass('open'); return; }
    from.css({
        opacity: '1',
        transition: ''
    });
    // The 100ms timeouts ensure the transition triggers
    setTimeout(function() {
        from.css({
            opacity: '0',
            transition: 'opacity 0.25s'
        });
        setTimeout(function() {
            var pho = parent.height();
            from.removeClass('open');
            to.addClass('open');
            var phn = parent.height();
            to.css({
                opacity: '0',
                transition: ''
            });
            parent.css({
                height: pho,
                transition: ''
            });
            setTimeout(function() {
                to.css({
                    opacity: '1',
                    transition: 'opacity 0.25s'
                });
                parent.css({ height: phn,
                             transition: 'height 0.25s',
                             overflow: 'hidden'
                           });
                setTimeout(function() {
                    parent.removeAttr('style');
                    from.removeAttr('style');
                    to.removeAttr('style');
                    finalCb();
                }, 350);
            }, 100);
         } , 250);
     }, 100);
};


var choose = function(ev) {
    ev.stopPropagation();
    var el = $(ev.target);
    var trs = $('[menu="' + $(ev.target).closest('menu').attr('id') + '"]');
    trs.each(function(_, tr) {
        var ch = $($(tr).children()[el.index()]);
        transition(ch.siblings('.open'), ch, function() {
            sidenote.setNotes();
            sidenote.fixNotes();
        });
    });
    menu.defaultHandlers(el);
};

$('[type="menu"]').each(function(_, el_) {
    var position = function(ev) {
        var mn = menu.getMenu(ev.target);
        mn.offset({top: ev.pageY, left: ev.pageX});
    };
    var addMenuHandlers = function(ev) {
        menu.getMenu(ev.target).children('.menu').children().off().click(choose);
    };

    var el = $(el_);
    el.click(position);
    el.click(addMenuHandlers);
});

});
})($, menu, sidenote);
