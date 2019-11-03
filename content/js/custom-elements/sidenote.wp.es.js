// @flow
/* eslint no-undef: "off" */

import { create, env } from 'sanctuary'

import {
  documentReadyPromise,
  fromNullableError,
  getBySelector,
  outerHeight,
  relative,
  removeElement,
  toMaybe
} from 'libs/util'

const S = create({ checkTypes: false, env })

const getReferrer = (el: HTMLElement): ?HTMLElement =>
  S.pipe([
    el => el.querySelectorAll('a'),
    Array.from,
    S.last,
    S.map(el => el.getAttribute('href')),
    S.chain(toMaybe),
    S.map(id => getBySelector('#' + id.slice(1))),
    S.maybeToNullable
  ])(el)

const cumulativeOffsetTop = (el: HTMLElement) => {
  const offsetParent = el.offsetParent
  if (offsetParent instanceof HTMLElement) {
    return offsetParent.tagName === 'MAIN' ? el.offsetTop : el.offsetTop + cumulativeOffsetTop(offsetParent)
  } else {
    throw new Error('Expected an `offsetParent`:' + el.outerHTML)
  }
}

const fixNotes = () => {
  S.reduce((prevBot: number) => (el: HTMLElement) => {
    const referrer = fromNullableError(`No referrer for ${el.toString()}`)(
      getReferrer(el)
    )
    const notedSpan = relative(el => el.previousElementSibling)(referrer)
    const top = S.max(cumulativeOffsetTop(notedSpan))(prevBot)
    el.style.top = top + 'px'
    return top + outerHeight(el)
  })(0)(Array.from(document.querySelectorAll('.sidenote:not(#warnings)')))
}

const setNotes = () => {
  const addSidenote = el => {
    // Putting block elements in a <p> auto-closes it so we put it immediately outside
    const referrer = getReferrer(el)
    if (referrer != null) {
      const noted = relative(el => el.previousElementSibling)(referrer)
      if (getComputedStyle(noted).display !== 'none') {
        const p = noted.closest('p')
        ;(p == null ? noted : p).insertAdjacentHTML(
          'beforebegin',
          '<aside class="sidenote">' + el.innerHTML + '</aside>'
        )
      }
    }
  }
  const delink = () => {
    document.querySelectorAll('.noted').forEach(el => {
      const nextSibling = relative(el => el.nextElementSibling)(el)
      nextSibling.style.display = 'none'
    })
    document.querySelectorAll('.sidenote').forEach(
      S.pipe([
        el => el.querySelectorAll('a'),
        Array.from,
        S.last,
        S.map(el => {
          el.style.display = 'none'
        })
      ])
    )
  }

  document.querySelectorAll('.footnotes').forEach(el => {
    el.style.display = 'none'
  })
  document
    .querySelectorAll('details')
    .forEach(el => observer.observe(el, { attributes: true }))
  document.querySelectorAll('.sidenote:not(#warnings)').forEach(removeElement)
  document.querySelectorAll('.footnotes > ol > li').forEach(addSidenote)
  delink()
}

const observer = new MutationObserver(fixNotes)

const removeNotes = () => {
  observer.disconnect()
  document.querySelectorAll('.sidenote:not(#warnings)').forEach(removeElement)
  document.querySelectorAll('.noted').forEach(el => {
    const nextSibling = relative(el => el.nextElementSibling)(el)
    nextSibling.style.display = ''
  })
  document.querySelectorAll('.footnotes').forEach(el => {
    el.style.display = ''
  })
}

const getFontSize = S.pipe([
  getComputedStyle,
  style => style['font-size'],
  Number.parseFloat
])

const addOrRemoveNotes = () => {
  const emWidth =
    window.innerWidth / parseFloat(getFontSize(getBySelector('html')))
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

documentReadyPromise.then(() => {
  if (!document.querySelector('.disable-sidenotes')) {
    addOrRemoveNotes()
    window.addEventListener('resize', addOrRemoveNotes)
  }
})

export default { setNotes, fixNotes }
