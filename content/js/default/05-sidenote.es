const sidenote = (($, MathJax) => {

const referrer = el => {
  const id = $(el).find('a').last().attr('href');
  if (id === undefined) {
    return [];
  } else {
    return $('[id="'+id.slice(1)+'"]');
  }
};

const fixNotes = () => {
  let prevBot = 0;
  const budge = (_, el_) => {
    const el = $(el_);
    el.offset((_, {top, left}) => {
      const pofs = referrer(el);
      if (pofs.length !== 0) {
        top = pofs.prev().offset().top;
      }
      if (top < prevBot) {
        top = prevBot;
      }
      prevBot = top + el.outerHeight(true);
      return {top: top, left: left};
    });
  };

  $('.sidenote').each(budge);
};

const setNotes = n => {
  const addSidenote = el => {
    //Putting block elements in a <p> auto-closes it
    let noted = referrer(el).prev();
    if (noted.is(':visible')) {
      const p = noted.closest('p');
      if (p.length !== 0) {
        noted = p;
      }
      noted.before('<aside class="sidenote">' + $(el).html() + '</aside>');
    }
  };
  const delink = () => {
    $('.noted').next().hide();
    $('.sidenote').each((_, el) => {
      $(el).find('a').last().hide();
    });
  };

  $('.sidenote').not('#warnings').remove();
  $('#article-title').before($('#warnings'));
  $('.footnotes > ol > li').each((_, el) => {
    addSidenote(el);
  });
  delink();
};

$(() => {
  $('.footnotes').hide();
  setNotes();
  $('details').each((_, el) => {
      (new MutationObserver(fixNotes)).observe(el, {attributes: true});
  });
  $('body').fontSpy({
    onFail: 'font-fail',
    callback: fixNotes
  });
  MathJax.Hub.Queue(() => {
    fixNotes();
  });
});

return {setNotes, fixNotes};
  
})($, MathJax);
