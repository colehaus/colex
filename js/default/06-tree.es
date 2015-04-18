// Use menus to build tree of choices
(($, menu, sidenote, MathJax) => {

const transition = (from, to, finalCb) => {
  if (typeof from[0] === 'undefined') { return; }
  const parent = to.parent();
  if (!parent.is(':visible')) {
    from.removeClass('open');
    to.addClass('open');
    return;
  }
  let start, prog, pho, phn;
  const dur1 = 300;
  const stage1 = timestamp => {
    start = start || timestamp;
    prog = (timestamp - start) / dur1;
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
  const dur2 = 300;
  const stage2 = timestamp => {
    prog = (timestamp - start) / dur2;
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
    
const choose = ev => {
  ev.stopPropagation();
  const el = $(ev.target);
  const trs = $('[menu="' + $(ev.target).closest('menu').attr('id') + '"]');
  trs.each((_, tr) => {
    const ch = $($(tr).children()[el.index()]);
    transition(ch.siblings('.open'), ch, () => {
      sidenote.setNotes();
      sidenote.fixNotes();
    });
  });
  menu.defaultHandlers(el);
};

$(() => {
  $('[type="menu"]').each((_, el_) => {
    const position = ({pageY, pageX, target}) => {
      const mn = menu.getMenu(target);
      mn.offset({top: pageY, left: pageX});
    };
    const addMenuHandlers = ({target}) => {
      menu.getMenu(target).children('.menu').children().off().click(choose);
    };
    $(el_).click(e => {position(e); addMenuHandlers(e);});
  });

  MathJax.Hub.Queue(() => {
    // Messes up rendering if we add to stylesheet
    $('.MathJax_MathContainer').css('display', 'inline');
    $('.MathJax_MathContainer > span').css('display', 'inline');
    // Re-inline fix rendering problem
    const inlines = $('.switch.inline > li.open');
    inlines.css('display', 'inline-block');
    inlines.offset(); // Trigger reflow
    inlines.css('display', 'inline');
    inlines.removeAttr('style');
  });
});

})($, menu, sidenote, MathJax);
