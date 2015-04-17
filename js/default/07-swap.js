// Doesn't currently handle nested swaps
(function ($, sidenote) {
$(function() {
'use strict';

var translations = function (top, bottom) {
    var bottomToTop = top.offset().top - bottom.offset().top;
    var between = bottom.height() - top.height();
    var topToBottom = bottom.offset().top - top.offset().top +
            (bottom.height() - top.height());
    return {
        bottomToTop: bottomToTop,
        between: between,
        topToBottom: topToBottom
    };
};

var distance = function (coord1, coord2) {
    return Math.sqrt(Math.pow(coord1.x - coord2.x, 2) +
                     Math.pow(coord1.y - coord2.y, 2));
};

var circleFromChord = function (startRads, stopRads, startCoord, stopCoord) {
    var radius = distance(startCoord, stopCoord) /
            Math.sin(Math.abs(stopRads - startRads) / 2) / 2;
    var center = { 
        x: startCoord.x - Math.cos(startRads) * radius, 
        y: startCoord.y - Math.sin(startRads) * radius
    };
    return {radius: radius, center: center};
};

var pointOnCircle = function (center, radius, angle) {
    return {
        x: center.x + Math.cos(angle) * radius,
        y: center.y + Math.sin(angle) * radius
    };
};

var swapTranslate = function (top, bottom, cb) {
    var between = top.nextUntil(bottom);
    var trans = translations(top, bottom);
  
    var angle = Math.PI/4;
    var mkTranslator = function (trans) {
        var startAngle;
        if (trans <= 0) {
            startAngle = Math.PI + angle;
        } else {
            startAngle = angle;
        }
        var circ = circleFromChord(startAngle,
                                   startAngle - 2 * angle,
                                   {x: 0, y: 0},
                                   {x: 0, y: trans});
        return function (prog) {
            var p = pointOnCircle(circ.center,
                                  circ.radius,
                                  startAngle - angle * 2 * prog);
            return 'translate(' + p.x + 'px, ' + -p.y + 'px)';
        };
    };
    var topTranslator = mkTranslator(trans.topToBottom);
    var bottomTranslator = mkTranslator(trans.bottomToTop);
    var betweenTranslator = mkTranslator(trans.between);
    
    var start;
    var dur = 300;
    var step = function (timestamp) {
        start = start || timestamp;
        var prog = (timestamp - start) / dur;
        if (prog <= 1) {
            top.css('transform', topTranslator(prog));
            bottom.css('transform', bottomTranslator(prog));
            between.css('transform', betweenTranslator(prog));
            requestAnimationFrame(step);
        } else {
            bottom.removeAttr('style');
            top.removeAttr('style');
            between.removeAttr('style');
            cb();
        }
    };
    requestAnimationFrame(step);
};

var swap = function () {
    var top, bottom;
    var arrow = $(this);
    var p = arrow.parent();
    var selector = '.' + p.attr('class').replace(' ', '.');
    if (arrow.attr('class') === 'swap-down') {
        top = p;
        bottom = p.nextAll(selector + ':first');
    } else {
        bottom = p;
        top = p.prevAll(selector + ':first');
    }
    swapTranslate(top, bottom, function() {
        swapDom(top, bottom);
        sidenote.fixNotes();
        decorate();
    });
};

var swapDom = function (a, b) {  
    var aNext = a.next();
    var aSib = aNext[0] === b[0] ? a : aNext;
    b.before(a);
    aSib.before(b);
};

// Assumes sorted
var groupBy = function (fn, ar) {
    var l = ar.length;
    var acc = [[ar[0]]];
    var accIndex = 0;
    for(var i = 1; i < l; i++) {
        if (fn(ar[i-1], ar[i])) {
            acc[accIndex].push(ar[i]);
        } else {
            accIndex += 1;
            acc[accIndex] = [ar[i]];
        }
    } 
    return acc;
};

var decorate = function () {
    var up = $('<span class="swap-up"></span>');
    var down = $('<span class="swap-down"></span>');
    var swaps = $('.swap');
    var swapGroups = groupBy(function (el1, el2) {
        return $(el1).attr("class") === $(el2).attr("class");
    }, swaps.toArray());
    var decorateGroup = function(gp) {
        var l = gp.length;
        gp.forEach(function (el, i) {
            var $el = $(el);
            if (i !== 0) {
                $el.prepend(up.clone());
            }
            if (i !== l - 1) {
                $el.append(down.clone());
            }
        });
    };
    $('.swap-up, .swap-down').remove();
    swapGroups.forEach(decorateGroup);
    $('.swap-up, .swap-down').click(swap);
};

decorate();

});
})($, sidenote);
