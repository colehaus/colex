'use strict';

var defaultHandlers = function(el) {
    var radioSelect = function() {
        var gp = el.attr('radiogroup');
        var mn = el.closest('menu');
        var sel = gp ? 'menuitem[radiogroup="'+gp+'"]' : 'menuitem:not([radiogroup])';
        mn.children(sel).removeAttr('checked');
        $(mn.children()[el.index()]).attr('checked', 'checked');
    };

    if (el.attr('type') === 'radio') { radioSelect(); }
    el.parent().remove();
};

$(function () {

var buildMenu = function(el) {
    var menuItem = function(el_) {
        var el = $(el_);
        var lb = el.attr('label');
        if (lb !== null) {
            var li = $('<li/>').text(lb);
            $(['onclick', 'checked', 'type', 'radiogroup']).each(function(_, at) {
                li.attr(at, el.attr(at));
            });
            return [li];
        } else {
            return [];
        }
    };
    var menu = function(el_) {
        var el = $(el_);
        var lb = el.attr('label');
        if (lb !== null) {
            return [$('<li>'+lb+'</li>').get()];
        } else {
            return [$('<hr/>').get()].concat(buildMenu(el), [$('<hr/>').get()]);
        }
    };

    var contents = $(el).children('menuitem, hr, menu').map(function(_, el) {
        switch (el.nodeName) {
            case 'MENUITEM':
              return menuItem(el);
              break;
            case 'HR':
              return [el];
              break;
            case 'MENU':
              return menu(el);
              break;
        }
    });
    var flattened = Array.prototype.concat.apply([], contents);
    var l = flattened.length;
    var finalContents = flattened.reduce(function(pv, cv, i) {
        var onEnd = i === 0 || i === (l - 1);
        var prevHr = typeof pv[i-1] !== 'undefined' && pv[i-1].nodeName === 'HR';
        var isEmpty = cv.nodeName === 'LI' && $(cv).text() === '';
        if ((onEnd || prevHr) && cv.nodeName === 'HR' || isEmpty) {
            return pv;
        } else {
            pv.push(cv);
            return pv;
        }
    }, []);
    return $('<ul class="menu"></ul>').append(finalContents);
};

var toggleMenu = function(ev) {
    ev.stopPropagation();
    var isPopup = function(_) {
        var el = $(this);
        var pe = el.parent().get(0);

        var isMenu = this.nodeName === 'MENU';
        var isSelfPop = el.attr('type') === 'popup';

        return isMenu && (isSelfPop || typeof pe !== 'undefined' && isPopup(pe));
    };

    var el = $(ev.target).closest('[type="menu"]');
    var mn = $('#'+el.attr('menu')).filter(isPopup);
    var ul = mn.children('ul.menu');
    if (ul.length === 0) {
        ul = buildMenu(mn);
        ul.children().click(function(ev) {
            defaultHandlers($(ev.target));
        });
        mn.append(ul);
    } else {
       ul.remove();
    }
};

    $('[type="menu"]').each(function(_, el) {
        $(el).click(toggleMenu);
    });
});