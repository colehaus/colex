'use strict';
$(function() {

var fixNotes = function() {
    var prevBot = 0;
    var positionCompare = function (el1, el2){
        var o1 = $(el1).offset();
        var o2 = $(el2).offset();
        if (o1.top === o2.top) {
            if (o1.left < o2.left) {
                return -1;
            }
            if (o1.left === o2.left) {
                return 0;
            }
            if (o1.left > o2.left) {
                return 1;
            }
        }
        if (o1.top < o2.top) {
            return -1;
        }
        if (o1.top > o2.top) {
            return 1;
        }
    };
    var budge = function (_, el_) {
        var el = $(el_);
        el.offset(function(_, ofs) {
            var pofs = el.parent('.noted').offset();
            if (pofs !== null) {
                ofs.top = pofs.top;
            }
            if (ofs.top < prevBot) {
                ofs.top = prevBot;
            }
            prevBot = ofs.top + ofs.height;
            return ofs;
        });
    };

    var notes = $('.noted').sort(positionCompare).map(function (_, el) {
        return $(el).children();
    });
    if ($('#warnings').length > 0) {
        $($('#warnings').concat(notes)).each(budge);
    } else {
        notes.each(budge);
    }
};

$('details').each(function (_, el) {
    (new MutationObserver (fixNotes)).observe(el, {attributes: true});
});
$('body').fontSpy({
    onFail: 'font-fail',
    callback: fixNotes
});

});
