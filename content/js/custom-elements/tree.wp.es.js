// @flow
/* eslint no-undef: "off" */

import { create, env } from 'sanctuary'

import menu from 'custom-elements/menu'
import sidenote from 'custom-elements/sidenote'
import {
  asHTMLElement,
  documentReadyPromise,
  getBySelector,
  makeAnimationPromise,
  parent,
  relative,
  toMaybe
} from 'libs/util'

const S = create({ checkTypes: false, env })

const interpolateNumber = (bot: number, top: number) => (fraction: number) =>
  (top - bot) * fraction + bot

type Event =
  | { tag: 'INVISIBLEPARENT', from: HTMLElement, to: HTMLElement }
  | { tag: 'SELECTEDOPEN' }
  | {
      tag: 'SELECTEDNEWVISIBLE',
      from: HTMLElement,
      to: HTMLElement,
      parent: HTMLElement
    }

const getHeight = S.pipe([
  getComputedStyle,
  style => style.height,
  Number.parseFloat
])

const handleEvent = (event: Event) => (resolve: Function, reject: Function) => {
  switch (event.tag) {
    case 'INVISIBLEPARENT':
      event.from.classList.remove('open')
      event.to.classList.add('open')
      resolve()
      break
    case 'SELECTEDOPEN':
      resolve()
      break
    case 'SELECTEDNEWVISIBLE':
      // Flow behaving badly
      const event_ = event
      const pho = getHeight(event_.parent)
      const stepDuration = 300 // ms
      const fadeOut = () =>
        makeAnimationPromise(stepDuration, prog => {
          event_.from.style.opacity = (1 - prog).toString()
        }).then(() => {
          event_.from.style.opacity = '0'
          event_.from.classList.remove('open')
        })
      const stretch = () =>
        Promise.resolve().then(() => {
          const heightFn = interpolateNumber(pho, getHeight(event_.parent))
          return makeAnimationPromise(stepDuration, prog => {
            event_.parent.style.height = heightFn(prog) + 'px'
          })
        })
      const fadeIn = () =>
        Promise.resolve().then(() => {
          event_.to.style.opacity = '0'
          event_.to.classList.add('open')
          return makeAnimationPromise(stepDuration, prog => {
            event_.to.style.opacity = prog.toString()
          })
        })
      const cleanUp = () => {
        event_.parent.removeAttribute('style')
        event_.from.removeAttribute('style')
        event_.to.removeAttribute('style')
      }
      fadeOut()
        .then(() => Promise.all([fadeIn(), stretch()]))
        .then(cleanUp)
        .then(resolve)
      break
    default:
      ;(event.tag: empty) // eslint-disable-line no-unused-expressions
      throw new Error(`Unexpected tag in ${event}`)
  }
}

const getIndexAmongSiblings = (el: HTMLElement): number =>
  S.pipe([
    parent,
    toMaybe,
    S.map(e => e.children),
    S.map(Array.from),
    S.map(els => els.findIndex(child => child === el)),
    S.fromMaybe(-1)
  ])(el)

const choose = (ev: MouseEvent) => {
  const el = asHTMLElement(ev.target)
  const menuEl = relative(el => el.closest('menu'))(el)
  S.map((contentTree: HTMLElement) => {
    const contentBranch: HTMLElement = relative(
      el_ => el_.children[getIndexAmongSiblings(el)]
    )(contentTree)
    const parentEl: HTMLElement = parent(contentBranch)
    const from: HTMLElement = relative(el => el.querySelector('.open'))(
      parentEl
    )
    const event = contentBranch.classList.contains('open')
      ? { tag: 'SELECTEDOPEN' }
      : getComputedStyle(parentEl).display !== 'none'
        ? {
          tag: 'SELECTEDNEWVISIBLE',
          from,
          to: contentBranch,
          parent: parentEl
        }
        : { tag: 'INVISIBLEPARENT', from, to: contentBranch }
    new Promise(handleEvent(event)).then(() => {
      sidenote.setNotes()
      sidenote.fixNotes()
    })
  })(Array.from(document.querySelectorAll(`[data-menu="${menuEl.id}"]`)))
  menu.defaultHandlers(el)
  return false
}

documentReadyPromise.then(() => {
  const params = new URLSearchParams(location.search)
  document.querySelectorAll('[type="menu"]').forEach(menu => {
    const id = menu.dataset.menu
    const label = params.get(id)
    if (label != null) {
      const menuItem = relative(el => el.querySelector(`[label="${label}"]`))(
        getBySelector('#' + id)
      )
      const from: HTMLElement = relative(el => el.querySelector('.open'))(menu)
      const to: HTMLElement = relative(
        el => el.children[getIndexAmongSiblings(menuItem)]
      )(menu)
      new Promise(handleEvent({ tag: 'INVISIBLEPARENT', from, to })).then(
        () => {
          sidenote.setNotes()
          sidenote.fixNotes()
        }
      )
    }
  })

  const menuEls = Array.from(document.querySelectorAll('[type="menu"]'))
  S.map(el => {
    el.addEventListener(
      'click',
      ({
        pageY,
        pageX,
        target
      }: {
        pageY: number,
        pageX: number,
        target: EventTarget
      }) => {
        const menuEl = menu.getMenu(asHTMLElement(target))
        const menuUlM: Maybe<HTMLElement> = toMaybe(
          menuEl.querySelector('ul.menu')
        )
        S.map((ul: HTMLElement) => {
          // CSS `top` and `left` set relative to containing block
          // pageX and pageY are relative to document
          // So we need to adjust for containing block
          const containingBlock = getBySelector('main')
          ul.style.left = pageX - containingBlock.offsetLeft + 'px'
          ul.style.top = pageY - containingBlock.offsetTop + 'px'
          Array.from(ul.children).forEach(el =>
            el.removeEventListener('click', choose)
          )
          Array.from(ul.children).forEach(el =>
            el.addEventListener('click', choose)
          )
        })(menuUlM)
      }
    )
  })(menuEls)

  MathJax.Hub.Queue(() => {
    // Messes up rendering if we add to stylesheet
    document.querySelectorAll('.MathJax_MathContainer').forEach(el => {
      getComputedStyle(el).display = 'inline'
    })
    document.querySelectorAll('.MathJax_MathContainer > span').forEach(el => {
      getComputedStyle(el).display = 'inline'
    })
    // Re-inline fix rendering problem
    const inlines = document.querySelectorAll('.switch.inline > li.open')
    inlines.forEach(el => {
      getComputedStyle(el).display = 'inline-block'
    })
    S.pipe([
      Array.from,
      S.head,
      S.map(e => e.getBoundingClientRect()) // Trigger reflow
    ])(inlines)
    inlines.forEach(el => {
      getComputedStyle(el).display = 'inline'
      el.removeAttribute('style')
    })
  })
})
