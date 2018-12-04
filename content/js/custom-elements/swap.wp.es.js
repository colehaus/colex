// @flow
/* eslint no-undef: "off" */

// Doesn't currently handle nested swaps

import { create, env } from 'sanctuary'

import sidenote from 'custom-elements/sidenote'
import {
  asHTMLElement,
  documentReadyPromise,
  makeAnimationPromise,
  parent,
  relative,
  removeElement
} from 'libs/util'
import { circleFromChord, pointOnCircle } from 'libs/geometry'

const S = create({ checkTypes: false, env })

const getHeight = S.pipe([
  getComputedStyle,
  style => style.height,
  Number.parseFloat
])

const prev = (el: HTMLElement) => (sel: string): HTMLElement =>
  S.pipe([
    parent,
    el => el.querySelectorAll(sel),
    Array.from,
    S.head,
    S.fromMaybe_(() => {
      throw new Error(`No prev element for ${el.toString()}`)
    })
  ])(el)

const next = (el: HTMLElement) => (sel: string): HTMLElement =>
  S.pipe([
    parent,
    el => el.querySelectorAll(sel),
    Array.from,
    S.dropWhile(child => child !== el),
    S.tail,
    S.chain(S.head),
    S.fromMaybe_(() => {
      throw new Error(`No next element for ${el.toString()}`)
    })
  ])(el)

const nextUntil = (left: HTMLElement) => (right: HTMLElement): Array<Node> =>
  S.pipe([
    parent,
    el => el.children,
    Array.from,
    S.dropWhile(child => child !== left),
    S.tail,
    S.fromMaybe([]),
    S.takeWhile(child => child !== right)
  ])(left)

const translations = (top: HTMLElement, bottom: HTMLElement) => {
  const bottomToTop =
    top.getBoundingClientRect().top - bottom.getBoundingClientRect().top
  const between = getHeight(bottom) - getHeight(top)
  const topToBottom =
    bottom.getBoundingClientRect().top -
    top.getBoundingClientRect().top +
    between
  return { bottomToTop, between, topToBottom }
}

const swapTranslate = (topEl: HTMLElement, bottomEl: HTMLElement) => (
  resolve: Function,
  reject: Function
) => {
  const betweenEls = S.map(asHTMLElement)(nextUntil(topEl)(bottomEl))
  const { bottomToTop, between, topToBottom } = translations(topEl, bottomEl)

  const angle = Math.PI / 4
  const mkTranslator = (trans: number): (number => string) => {
    const startAngle = trans <= 0 ? Math.PI + angle : angle
    const { center, radius } = circleFromChord(
      startAngle,
      startAngle - 2 * angle,
      { x: 0, y: 0 },
      { x: 0, y: trans }
    )
    return (prog: number): string => {
      const { x, y } = pointOnCircle(
        center,
        radius,
        startAngle - angle * 2 * prog
      )
      return `translate(${x}px, ${-y}px)`
    }
  }
  const topTranslator = mkTranslator(topToBottom)
  const bottomTranslator = mkTranslator(bottomToTop)
  const betweenTranslator = mkTranslator(between)

  const stepDuration = 300 // ms

  Promise.all([
    makeAnimationPromise(stepDuration, prog => {
      topEl.style.transform = topTranslator(prog)
    }).then(() => topEl.removeAttribute('style')),
    makeAnimationPromise(stepDuration, prog => {
      bottomEl.style.transform = bottomTranslator(prog)
    }).then(() => bottomEl.removeAttribute('style')),
    makeAnimationPromise(stepDuration, prog =>
      betweenEls.forEach(el => {
        el.style.transform = betweenTranslator(prog)
      })
    ).then(() => betweenEls.forEach(el => el.removeAttribute('style')))
  ]).then(resolve)
}

const swap = function () {
  const arrow = this
  const p = parent(arrow)
  const selector = '.' + Array.from(p.classList).join('.')
  const [top, bottom] =
    arrow.getAttribute('class') === 'swap-down'
      ? [p, next(p)(selector)]
      : [prev(p)(selector), p]
  new Promise(swapTranslate(top, bottom)).then(() => {
    swapDom(top, bottom)
    sidenote.fixNotes()
    document.querySelectorAll('.swap-up, .swap-down').forEach(removeElement)
    decorate()
  })
}

const swapDom = (top: HTMLElement, bot: HTMLElement) => {
  const topNext = relative(el => el.nextElementSibling)(top)
  if (topNext === bot) {
    parent(bot).insertBefore(top, bot)
    parent(top).insertBefore(bot, top)
  } else {
    parent(bot).insertBefore(top, bot)
    parent(topNext).insertBefore(bot, topNext)
  }
}

const decorate = () => {
  const up = document.createElement('span')
  up.classList.add('swap-up')
  const down = document.createElement('span')
  down.classList.add('swap-down')
  const swaps = document.querySelectorAll('.swap')
  const swapGroups = S.groupBy(S.on(S.equals)(el => el.className))(
    Array.from(swaps)
  )
  const decorateGroup = group => {
    S.pipe([
      S.tail,
      S.map(S.map(el => el.insertBefore(up.cloneNode(true), el.firstChild)))
    ])(group)
    S.pipe([S.init, S.map(S.map(el => el.appendChild(down.cloneNode(true))))])(
      group
    )
  }
  swapGroups.forEach(decorateGroup)
  document
    .querySelectorAll('.swap-up, .swap-down')
    .forEach(el => el.addEventListener('click', swap))
}

documentReadyPromise.then(decorate)
