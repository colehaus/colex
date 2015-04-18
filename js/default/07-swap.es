// Doesn't currently handle nested swaps
(($, sidenote) => {

const translations = (top, bottom) => {
  const bottomToTop = top.offset().top - bottom.offset().top;
  const between = bottom.height() - top.height();
  const topToBottom = bottom.offset().top - top.offset().top +
    (bottom.height() - top.height());
  return {bottomToTop, between, topToBottom};
};

const distance = (coord1, coord2) => 
  Math.sqrt(Math.pow(coord1.x - coord2.x, 2) +
            Math.pow(coord1.y - coord2.y, 2));

const circleFromChord = (startRads, stopRads, startCoord, stopCoord) => {
  const radius = distance(startCoord, stopCoord) /
    Math.sin(Math.abs(stopRads - startRads) / 2) / 2;
  const center = { 
    x: startCoord.x - Math.cos(startRads) * radius, 
    y: startCoord.y - Math.sin(startRads) * radius
  };
  return {radius, center};
};

const pointOnCircle = ({x, y}, radius, angle) => ({
  x: x + Math.cos(angle) * radius,
  y: y + Math.sin(angle) * radius
});

const swapTranslate = (topEl, bottomEl, cb) => {
  const betweenEls = topEl.nextUntil(bottomEl);
  const {bottomToTop, between, topToBottom} = translations(topEl, bottomEl);
  
  const angle = Math.PI/4;
  const mkTranslator = trans => {
    let startAngle;
    if (trans <= 0) {
      startAngle = Math.PI + angle;
    } else {
      startAngle = angle;
    }
    const {center, radius} = circleFromChord(startAngle,
                                             startAngle - 2 * angle,
                                             {x: 0, y: 0},
                                             {x: 0, y: trans});
    return prog => {
      const {x, y} = pointOnCircle(center, radius,
                                   startAngle - angle * 2 * prog);
      return 'translate(' + x + 'px, ' + -y + 'px)';
    };
  };
  const topTranslator = mkTranslator(topToBottom);
  const bottomTranslator = mkTranslator(bottomToTop);
  const betweenTranslator = mkTranslator(between);
  
  let start, prog;
  const dur = 300;
  const step = timestamp => {
    start = start || timestamp;
    prog = (timestamp - start) / dur;
    if (prog <= 1) {
      topEl.css('transform', topTranslator(prog));
      bottomEl.css('transform', bottomTranslator(prog));
      betweenEls.css('transform', betweenTranslator(prog));
      requestAnimationFrame(step);
    } else {
      topEl.removeAttr('style');
      bottomEl.removeAttr('style');
      betweenEls.removeAttr('style');
      cb();
    }
  };
  requestAnimationFrame(step);
};

const swap = function () {
  let top, bottom;
  const arrow = $(this);
  const p = arrow.parent();
  const selector = '.' + p.attr('class').replace(' ', '.');
  if (arrow.attr('class') === 'swap-down') {
    top = p;
    bottom = p.nextAll(selector + ':first');
  } else {
    bottom = p;
    top = p.prevAll(selector + ':first');
  }
  swapTranslate(top, bottom, () => {
    swapDom(top, bottom);
    sidenote.fixNotes();
    decorate();
  });
};

const swapDom = (a, b) => {  
  const aNext = a.next();
  const aSib = aNext[0] === b[0] ? a : aNext;
  b.before(a);
  aSib.before(b);
};

// Assumes sorted
const groupBy = (fn, ar) => {
  const l = ar.length;
  let acc = [[ar[0]]];
  let accIndex = 0;
  for (let i = 1; i < l; i++) {
    if (fn(ar[i-1], ar[i])) {
      acc[accIndex].push(ar[i]);
    } else {
      accIndex += 1;
      acc[accIndex] = [ar[i]];
    }
  } 
  return acc;
};

const decorate = () => {
  const up = $('<span class="swap-up"></span>');
  const down = $('<span class="swap-down"></span>');
  const swaps = $('.swap');
  const swapGroups = groupBy((el1, el2) =>
    $(el1).attr("class") === $(el2).attr("class"),
    swaps.toArray());
  const decorateGroup = gp => {
    const l = gp.length;
    gp.forEach((el, i) => {
      const $el = $(el);
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

$(() => {decorate();});

})($, sidenote);
