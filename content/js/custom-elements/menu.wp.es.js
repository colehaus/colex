import $ from 'jquery'

const defaultHandlers = el => {
  if (el.attr('type') === 'radio') {
    const gp = el.attr('radiogroup');
    const mn = el.closest('menu');
    const sel = gp ? 'menuitem[radiogroup="'+gp+'"]' : 'menuitem:not([radiogroup])';
    mn.children(sel).removeAttr('checked');
    $(mn.children()[el.index()]).attr('checked', 'checked');
  }
  el.parent().remove();
};
const getMenu = el => $('#'+ $(el).closest('[type="menu"]').attr('data-menu'));

const buildMenu = el => {
  const menuItem = el_ => {
    const el = $(el_);
    const lb = el.attr('label');
    if (lb !== null) {
      const li = $('<li/>').text(lb);
      $(['onclick', 'checked', 'type', 'radiogroup']).each((_, at) => {
        li.attr(at, el.attr(at));
      });
      return [li];
    } else {
      return [];
    }
  };
  const menu = (el_) => {
    const el = $(el_);
    const lb = el.attr('label');
    if (lb !== null) {
      return [$('<li>'+lb+'</li>').get()];
    } else {
      return [$('<hr/>').get()].concat(buildMenu(el), [$('<hr/>').get()]);
    }
  };

  const contents = $(el).children('menuitem, hr, menu').map((_, el) => {
    switch (el.nodeName) {
    case 'MENUITEM':
      return menuItem(el);
    case 'HR':
      return [el];
    case 'MENU':
      return menu(el);
    }
  });
  const flattened = Array.prototype.concat.apply([], contents);
  const l = flattened.length;
  const finalContents = flattened.reduce((pv, cv, i) => {
    const onEnd = i === 0 || i === (l - 1);
    const prevHr = typeof pv[i-1] !== 'undefined' && pv[i-1].nodeName === 'HR';
    const isEmpty = cv.nodeName === 'LI' && $(cv).text() === '';
    if ((onEnd || prevHr) && cv.nodeName === 'HR' || isEmpty) {
      return pv;
    } else {
      pv.push(cv);
      return pv;
    }
  }, []);
  return $('<ul class="menu"></ul>').append(finalContents);
};

const toggleMenu = ev => {
  // Menu shouldn't popup when clicking on interactive element
  const isInter = [
    'TEXTAREA',
    'BUTTON',
    'INPUT',
    'OPTION',
    'SELECT',
    'DETAILS',
    'SUMMARY',
    'A'
  ].some(tg => ev.target.nodeName == tg);
  if (isInter) {return;}

  ev.stopPropagation();
  const isPopup = function () {
    const el = $(this);
    const pe = el.parent().get(0);

    const isMenu = this.nodeName === 'MENU';
    const isSelfPop = el.attr('type') === 'popup';

    return isMenu && (isSelfPop || typeof pe !== 'undefined' && isPopup(pe));
  };

  const mn = getMenu(ev.target).filter(isPopup);
  let ul = mn.children('ul.menu');
  $('ul.menu').remove();
  if (ul.length === 0) {
    ul = buildMenu(mn);
    ul.children().click(ev => {
      defaultHandlers($(ev.target));
    });
    mn.append(ul);
  }
};

$(() => {
  $('[type="menu"]').each((_, el) => {
    $(el).click(toggleMenu);
  });
});

export default {getMenu, defaultHandlers}
