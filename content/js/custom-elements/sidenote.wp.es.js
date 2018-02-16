// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import {create, env} from 'sanctuary'
const S = create({checkTypes: false, env})

const getReferrer = (el: JQuery): ?JQuery =>
  S.pipe([
    S.toMaybe,
    S.map(id => $('#' + id.slice(1))),
    S.maybeToNullable
  ])(
    el.find('a').last().attr('href')
  )

const fixNotes = () => {
  S.reduce(
    prevBot => _el => {
      const el = $(_el)
      el.offset((_1, {top, left}) =>
        S.pipe([
          S.toMaybe,
          S.maybe_(() => {
            prevBot = top + el.outerHeight(true)
            return {top, left}
          })(
            ref => {
              const top = S.max(ref.prev().offset().top)(prevBot)
              prevBot = top + el.outerHeight(true)
              return {top, left}
            }
          )
        ])(
          getReferrer(el)
        )
      )
      return prevBot
    })(
    0)(
    $('.sidenote').toArray())
}

const setNotes = () => {
  const addSidenote = el => {
    // Putting block elements in a <p> auto-closes it so we put it immediately outside
    const referrer = getReferrer(el)
    if (referrer != null) {
      const noted = referrer.prev()
      if (noted.is(':visible')) {
        const p = noted.closest('p');
        (p.length === 0 ? noted : p).before('<aside class="sidenote">' + $(el).html() + '</aside>')
      }
    }
  }
  const delink = () => {
    $('.noted').next().hide()
    $('.sidenote').each((_, el) => {
      $(el).find('a').last().hide()
    })
  }

  $('.footnotes').hide()
  $('details').each((_, el) => {
    observer.observe(el, {attributes: true})
  })
  $('.sidenote').not('#warnings').remove()
  $('.footnotes > ol > li').each((_, el) => {
    addSidenote($(el))
  })
  delink()
}

const observer = new MutationObserver(fixNotes)

const removeNotes = () => {
  observer.disconnect()
  $('.sidenote').not('#warnings').remove()
  $('.noted').next().show()
  $('.footnotes').show()
}

const addOrRemoveNotes = () => {
  const emWidth = $(window).width() / parseFloat($('html').css('font-size'))
  if (emWidth > 60) {
    setNotes()
    // $FlowFixMe
    document.fonts.ready.then(fixNotes)
    MathJax.Hub.Queue(() => {
      fixNotes()
    })
  } else {
    removeNotes()
  }
}

$(() => {
  addOrRemoveNotes()
  $(window).resize(addOrRemoveNotes)
})

export default {setNotes, fixNotes}
