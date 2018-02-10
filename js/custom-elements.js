webpackJsonp([1],{

/***/ 11:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery__ = __webpack_require__(0);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_jquery__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary__ = __webpack_require__(1);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_sanctuary__);

/* eslint no-undef: "off" */



const S = Object(__WEBPACK_IMPORTED_MODULE_1_sanctuary__["create"])({ checkTypes: false, env: __WEBPACK_IMPORTED_MODULE_1_sanctuary__["env"] });

const defaultHandlers = el => {
  if (el.attr('type') === 'radio') {
    const menu = el.closest('menu');
    const item = menu.find(`[label="${el.text()}"]`);
    handleEvent({ tag: 'ITEMSELECT', menu, item });
  }
};

const getMenu = el => __WEBPACK_IMPORTED_MODULE_0_jquery___default()('#' + __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).closest('[type="menu"]').attr('data-menu'));

const renderMenuItem = menuItem => {
  switch (menuItem.tag) {
    case 'HR':
      return __WEBPACK_IMPORTED_MODULE_0_jquery___default()('<hr/>');
    case 'MENUITEM':
      const li = __WEBPACK_IMPORTED_MODULE_0_jquery___default()('<li/>').text(menuItem.label).attr('type', menuItem.type);
      if (menuItem.active) {
        li.attr('checked', 'checked');
      } else {
        li.removeAttr('checked');
      }
      return li;
    default:
      throw Error(menuItem.toString());
  }
};

const renderMenu = menu => {
  switch (menu.tag) {
    case 'CLOSED':
      return null;
    case 'OPEN':
      return __WEBPACK_IMPORTED_MODULE_0_jquery___default()('<ul class="menu"></ul>').append(S.map(renderMenuItem)(menu.items));
  }
};

// The JQuery declaration seems to be wrong
const fixTarget = target => HTMLElement = target; // eslint-disable-line no-return-assign

const parseMenuItem = menuItem => {
  switch (menuItem.get(0).nodeName) {
    case 'HR':
      return { tag: 'HR' };
    case 'MENUITEM':
      return { tag: 'MENUITEM',
        label: menuItem.attr('label'),
        type: menuItem.attr('type'),
        active: menuItem.attr('checked') === 'checked'
      };
    default:
      throw Error(menuItem.toString());
  }
};

const parseMenu = menu => menu.data('active') ? { tag: 'CLOSED' } : { tag: 'OPEN',
  items: S.map(el => parseMenuItem(__WEBPACK_IMPORTED_MODULE_0_jquery___default()(el)))(menu.children('menuitem, hr').toArray())
};

const handleEvent = event => {
  switch (event.tag) {
    case 'MENUCLICK':
      S.pipe([parseMenu, renderMenu, S.toMaybe, S.maybe_(() => {
        __WEBPACK_IMPORTED_MODULE_0_jquery___default()('ul.menu').remove();
        event.menu.data('active', false);
      })(menuDom => {
        event.menu.append(menuDom);
        event.menu.data('active', true);
      })])(event.menu);
      break;
    case 'ITEMSELECT':
      __WEBPACK_IMPORTED_MODULE_0_jquery___default()('ul.menu').remove();
      event.menu.data('active', false);
      S.map(el => __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).removeAttr('checked'))(event.menu.children().toArray());
      event.item.attr('checked', 'checked');
      break;
  }
};

const toggleMenu = ev => {
  // Menu shouldn't popup when clicking on interactive element
  const isInter = ['TEXTAREA', 'BUTTON', 'INPUT', 'OPTION', 'SELECT', 'DETAILS', 'SUMMARY', 'A'].some(tg => fixTarget(ev.target).nodeName === tg);
  if (isInter) {
    return;
  }
  const isPopup = el => __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).get(0).nodeName === 'MENU' && __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).attr('type') === 'popup';
  const menu = getMenu(fixTarget(ev.target)).filter((_, el) => isPopup(el));
  if (menu != null) {
    handleEvent({ tag: 'MENUCLICK', menu: __WEBPACK_IMPORTED_MODULE_0_jquery___default()(menu) });
  }
};

__WEBPACK_IMPORTED_MODULE_0_jquery___default()(() => {
  __WEBPACK_IMPORTED_MODULE_0_jquery___default()('[type="menu"]').each((_, el) => {
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).click(toggleMenu);
  });
});

/* harmony default export */ __webpack_exports__["a"] = ({ getMenu, defaultHandlers });

/***/ }),

/***/ 26:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_custom_elements_tree__ = __webpack_require__(27);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_custom_elements_swap__ = __webpack_require__(28);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_custom_elements_sidenote__ = __webpack_require__(8);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_custom_elements_menu__ = __webpack_require__(11);





/***/ }),

/***/ 27:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery__ = __webpack_require__(0);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_jquery__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary__ = __webpack_require__(1);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_sanctuary__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_custom_elements_menu__ = __webpack_require__(11);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_custom_elements_sidenote__ = __webpack_require__(8);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_libs_util__ = __webpack_require__(7);

/* eslint no-undef: "off" */








const S = Object(__WEBPACK_IMPORTED_MODULE_1_sanctuary__["create"])({ checkTypes: false, env: __WEBPACK_IMPORTED_MODULE_1_sanctuary__["env"] });

const handleEvent = event => (resolve, reject) => {
  switch (event.tag) {
    case 'INVISIBLEPARENT':
      event.from.removeClass('open');
      event.to.addClass('open');
      resolve();
      break;
    case 'SELECTEDOPEN':
      resolve();
      break;
    case 'SELECTEDNEWVISIBLE':
      // Flow behaving badly
      const event_ = event;
      Object(__WEBPACK_IMPORTED_MODULE_4_libs_util__["a" /* makeAnimationPromise */])(300, prog => event_.from.css('opacity', 1 - prog)).then(() => {
        const pho = event_.parent.height();
        event_.from.css('opacity', 0);
        event_.to.css('opacity', 0);
        event_.from.removeClass('open');
        event_.to.addClass('open');
        const phn = event_.parent.height();
        return [pho, phn];
      }).then(([pho, phn]) => Object(__WEBPACK_IMPORTED_MODULE_4_libs_util__["a" /* makeAnimationPromise */])(300, prog => {
        event_.to.css('opacity', prog);
        event_.parent.css('height', pho + (phn - pho) * prog);
      })).then(() => {
        event_.parent.removeAttr('style');
        event_.from.removeAttr('style');
        event_.to.removeAttr('style');
      }).then(resolve);
      break;
  }
};

const choose = ev => {
  ev.stopPropagation();
  const el = __WEBPACK_IMPORTED_MODULE_0_jquery___default()(ev.target);
  S.map(contentTree => {
    const contentBranch = __WEBPACK_IMPORTED_MODULE_0_jquery___default()(__WEBPACK_IMPORTED_MODULE_0_jquery___default()(contentTree).children().get(el.index()));
    const event = contentBranch.hasClass('open') ? { tag: 'SELECTEDOPEN' } : contentBranch.parent().is(':visible') ? { tag: 'SELECTEDNEWVISIBLE', from: contentBranch.siblings('.open'), to: contentBranch, parent: contentBranch.parent() } : { tag: 'INVISIBLEPARENT', from: contentBranch.siblings('.open'), to: contentBranch };
    new Promise(handleEvent(event)).then(() => {
      __WEBPACK_IMPORTED_MODULE_3_custom_elements_sidenote__["a" /* default */].setNotes();
      __WEBPACK_IMPORTED_MODULE_3_custom_elements_sidenote__["a" /* default */].fixNotes();
    });
  })(__WEBPACK_IMPORTED_MODULE_0_jquery___default()(`[data-menu="${el.closest('menu').attr('id')}"]`).toArray());
  __WEBPACK_IMPORTED_MODULE_2_custom_elements_menu__["a" /* default */].defaultHandlers(el);
};

const fixTarget = target => HTMLElement = target; // eslint-disable-line no-return-assign

__WEBPACK_IMPORTED_MODULE_0_jquery___default()(() => {
  S.map(el => __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).click(({ pageY, pageX, target }) => __WEBPACK_IMPORTED_MODULE_2_custom_elements_menu__["a" /* default */].getMenu(fixTarget(target)).offset({ top: pageY, left: pageX }).children('ul.menu').children().off().click(choose)))(__WEBPACK_IMPORTED_MODULE_0_jquery___default()('[type="menu"]').toArray());

  MathJax.Hub.Queue(() => {
    // Messes up rendering if we add to stylesheet
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.MathJax_MathContainer').css('display', 'inline');
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.MathJax_MathContainer > span').css('display', 'inline');
    // Re-inline fix rendering problem
    const inlines = __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.switch.inline > li.open');
    inlines.css('display', 'inline-block');
    inlines.offset(); // Trigger reflow
    inlines.css('display', 'inline');
    inlines.removeAttr('style');
  });
});

/***/ }),

/***/ 28:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery__ = __webpack_require__(0);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_jquery__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary__ = __webpack_require__(1);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_sanctuary__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_custom_elements_sidenote__ = __webpack_require__(8);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_libs_util__ = __webpack_require__(7);

/* eslint no-undef: "off" */

// Doesn't currently handle nested swaps







const S = Object(__WEBPACK_IMPORTED_MODULE_1_sanctuary__["create"])({ checkTypes: false, env: __WEBPACK_IMPORTED_MODULE_1_sanctuary__["env"] });

const translations = (top, bottom) => {
  const bottomToTop = top.offset().top - bottom.offset().top;
  const between = bottom.height() - top.height();
  const topToBottom = bottom.offset().top - top.offset().top + between;
  return { bottomToTop, between, topToBottom };
};

const distance = (coord1, coord2) => Math.hypot(coord1.x - coord2.x, coord1.y - coord2.y);

const circleFromChord = (startRads, stopRads, startCoord, stopCoord) => {
  const radius = distance(startCoord, stopCoord) / Math.sin(Math.abs(stopRads - startRads) / 2) / 2;
  const center = pointOnCircle(startCoord, radius, startRads + Math.PI);
  return { radius, center };
};

const pointOnCircle = ({ x, y }, radius, angle) => ({
  x: x + Math.cos(angle) * radius,
  y: y + Math.sin(angle) * radius
});

const swapTranslate = (topEl, bottomEl) => (resolve, reject) => {
  const betweenEls = topEl.nextUntil(bottomEl);
  const { bottomToTop, between, topToBottom } = translations(topEl, bottomEl);

  const angle = Math.PI / 4;
  const mkTranslator = trans => {
    const startAngle = trans <= 0 ? Math.PI + angle : angle;
    const { center, radius } = circleFromChord(startAngle, startAngle - 2 * angle, { x: 0, y: 0 }, { x: 0, y: trans });
    return prog => {
      const { x, y } = pointOnCircle(center, radius, startAngle - angle * 2 * prog);
      return `translate(${x}px, ${-y}px)`;
    };
  };
  const topTranslator = mkTranslator(topToBottom);
  const bottomTranslator = mkTranslator(bottomToTop);
  const betweenTranslator = mkTranslator(between);

  Object(__WEBPACK_IMPORTED_MODULE_3_libs_util__["a" /* makeAnimationPromise */])(300, prog => {
    topEl.css('transform', topTranslator(prog));
    bottomEl.css('transform', bottomTranslator(prog));
    betweenEls.css('transform', betweenTranslator(prog));
  }).then(() => {
    topEl.removeAttr('style');
    bottomEl.removeAttr('style');
    betweenEls.removeAttr('style');
  }).then(resolve);
};

const swap = function () {
  const arrow = __WEBPACK_IMPORTED_MODULE_0_jquery___default()(this);
  const p = arrow.parent();
  const selector = '.' + p.attr('class').replace(' ', '.');
  const [top, bottom] = arrow.attr('class') === 'swap-down' ? [p, p.nextAll(selector + ':first')] : [p.prevAll(selector + ':first'), p];
  new Promise(swapTranslate(top, bottom)).then(() => {
    swapDom(top, bottom);
    __WEBPACK_IMPORTED_MODULE_2_custom_elements_sidenote__["a" /* default */].fixNotes();
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.swap-up, .swap-down').remove();
    decorate();
  });
};

const swapDom = (top, bot) => {
  const topNext = top.next();
  if (topNext[0] === bot[0]) {
    bot.before(top);
    top.before(bot);
  } else {
    bot.before(top);
    topNext.before(bot);
  }
};

const decorate = () => {
  const up = __WEBPACK_IMPORTED_MODULE_0_jquery___default()('<span class="swap-up"></span>');
  const down = __WEBPACK_IMPORTED_MODULE_0_jquery___default()('<span class="swap-down"></span>');
  const swaps = __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.swap');
  const swapGroups = S.groupBy(S.on(S.equals)(el => __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).attr('class')))(swaps.toArray());
  const decorateGroup = group => {
    S.pipe([S.tail, S.map(S.map(el => __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).prepend(up.clone())))])(group);
    S.pipe([S.init, S.map(S.map(el => __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).append(down.clone())))])(group);
  };
  swapGroups.forEach(decorateGroup);
  __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.swap-up, .swap-down').click(swap);
};

__WEBPACK_IMPORTED_MODULE_0_jquery___default()(decorate);

/***/ }),

/***/ 8:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery__ = __webpack_require__(0);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_jquery___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_jquery__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary__ = __webpack_require__(1);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_sanctuary___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_sanctuary__);

/* eslint no-undef: "off" */



const S = Object(__WEBPACK_IMPORTED_MODULE_1_sanctuary__["create"])({ checkTypes: false, env: __WEBPACK_IMPORTED_MODULE_1_sanctuary__["env"] });

const getReferrer = el => S.pipe([S.toMaybe, S.map(id => __WEBPACK_IMPORTED_MODULE_0_jquery___default()('#' + id.slice(1))), S.maybeToNullable])(el.find('a').last().attr('href'));

const fixNotes = () => {
  S.reduce(prevBot => _el => {
    const el = __WEBPACK_IMPORTED_MODULE_0_jquery___default()(_el);
    el.offset((_1, { top, left }) => S.pipe([S.toMaybe, S.maybe({ top, left })(ref => {
      const top = S.max(ref.prev().offset().top)(prevBot);
      prevBot = top + el.outerHeight(true);
      return { top, left };
    })])(getReferrer(el)));
    return prevBot;
  })(0)(__WEBPACK_IMPORTED_MODULE_0_jquery___default()('.sidenote').toArray());
};

const setNotes = () => {
  const addSidenote = el => {
    // Putting block elements in a <p> auto-closes it so we put it immediately outside
    const referrer = getReferrer(el);
    if (referrer != null) {
      const noted = referrer.prev();
      if (noted.is(':visible')) {
        const p = noted.closest('p');
        (p.length === 0 ? noted : p).before('<aside class="sidenote">' + __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).html() + '</aside>');
      }
    }
  };
  const delink = () => {
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.noted').next().hide();
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.sidenote').each((_, el) => {
      __WEBPACK_IMPORTED_MODULE_0_jquery___default()(el).find('a').last().hide();
    });
  };

  __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.sidenote').not('#warnings').remove();
  __WEBPACK_IMPORTED_MODULE_0_jquery___default()('#article-title').before(__WEBPACK_IMPORTED_MODULE_0_jquery___default()('#warnings'));
  __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.footnotes > ol > li').each((_, el) => {
    addSidenote(__WEBPACK_IMPORTED_MODULE_0_jquery___default()(el));
  });
  delink();
};

__WEBPACK_IMPORTED_MODULE_0_jquery___default()(() => {
  if (__WEBPACK_IMPORTED_MODULE_0_jquery___default()(window).width() > 850) {
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()('.footnotes').hide();
    setNotes();
    __WEBPACK_IMPORTED_MODULE_0_jquery___default()('details').each((_, el) => {
      new MutationObserver(fixNotes).observe(el, { attributes: true });
    });
    // $FlowFixMe
    document.fonts.ready.then(fixNotes);
    MathJax.Hub.Queue(() => {
      fixNotes();
    });
  }
});

/* harmony default export */ __webpack_exports__["a"] = ({ setNotes, fixNotes });

/***/ })

},[26]);
//# sourceMappingURL=custom-elements.js.map