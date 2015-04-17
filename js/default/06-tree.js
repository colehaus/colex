// Use menus to build tree of choices
(function($, menu, sidenote, MathJax) {
$(function() {
'use strict';
    
var transition = function(from, to, finalCb) {
    if(typeof from[0] === 'undefined') { return; }
    var parent = to.parent();
    if (!parent.is(':visible')) {
        from.removeClass('open');
        to.addClass('open');
        return;
    }
    var start, pho, phn;
    var dur1 = 300;
    var dur2 = 300;
    var stage1 = function (timestamp) {
        start = start || timestamp;
        var prog = (timestamp - start) / dur1;
        if (prog <= 1) {
            from.css('opacity', 1 - prog);
            requestAnimationFrame(stage1);
        } else {
            pho = parent.height();
            from.css('opacity', 0);
            to.css('opacity', 0);
            from.removeClass('open');
            to.addClass('open');
            phn = parent.height();
            parent.css('overflow-y', 'hidden');
            start = timestamp;
            requestAnimationFrame(stage2);
        }
        return;
    };
    var stage2 = function (timestamp) {
        var prog = (timestamp - start) / dur2;
        if (prog <= 1) {
            to.css('opacity', prog);
            parent.css('height', pho + (phn - pho) * prog);
            requestAnimationFrame(stage2);
        } else {
            parent.removeAttr('style'); 
            from.removeAttr('style'); 
            to.removeAttr('style'); 
            finalCb();
        }
        return;
    };
    requestAnimationFrame(stage1);
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

MathJax.Hub.Queue(function () {
    // Messes up rendering if we add to stylesheet
    $('.MathJax_MathContainer').css('display', 'inline');
    $('.MathJax_MathContainer > span').css('display', 'inline');
    // Re-inline fix rendering problem
    var inlines = $('.switch.inline > li.open');
    inlines.css('display', 'inline-block');
    inlines.offset(); // Trigger reflow
    inlines.css('display', 'inline');
    inlines.removeAttr('style');
});

});
})($, menu, sidenote, MathJax);
