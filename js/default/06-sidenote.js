var sidenote = (function($, MathJax) {
'use strict';

var referrer = function(el) {
    var id = $(el).find('a').last().attr('href');
    if (id === undefined) {
        return [];
    } else {
        return $('[id="'+id.slice(1)+'"]');
    }
};

var fixNotes = function() {
    var prevBot = 0;

    var budge = function (_, el_) {
        var el = $(el_);
        el.offset(function(_, ofs) {
            var top = ofs.top;
            var pofs = referrer(el);
            if (pofs.length !== 0) {
                top = pofs.prev().offset().top;
            }
            if (top < prevBot) {
                top = prevBot;
            }
            prevBot = top + el.outerHeight(true);
            return {top: top, left: ofs.left};
        });
    };

    $('.sidenote').each(budge);
};

var setNotes = function(n) {
    var addSidenote = function(el) {
        //Putting block elements in a <p> auto-closes it
        var noted = referrer(el).prev();
        if (noted.is(':visible')) {
            var p = noted.closest('p');
            if (p.length !== 0) {
                noted = p;
            }
            noted.before('<aside class="sidenote">' + $(el).html() + '</aside>');
        }
    };
    var delink = function() {
        $('.noted').next().hide();
        $('.sidenote').each(function(_, el) {
            $(el).find('a').last().hide();
        });
    };

    $('.sidenote').not('#warnings').remove();
    $('#article-title').before($('#warnings'));
    $('.footnotes > ol > li').each(function(_, el) {
        addSidenote(el);
    });
    delink();
};

$(function() {
    $('.footnotes').hide();
    setNotes();
    $('details').each(function (_, el) {
        (new MutationObserver (fixNotes)).observe(el, {attributes: true});
    });
    $('body').fontSpy({
        onFail: 'font-fail',
        callback: fixNotes
    });
    MathJax.Hub.Queue(function () {
        fixNotes();
    });
});

return {
    setNotes: setNotes,
    fixNotes: fixNotes
};

})($, MathJax);
