// @flow
/* eslint no-undef: "off" */

import { create, env } from 'sanctuary'

import { documentReadyPromise, toMaybe } from 'libs/util'

const S = create({ checkTypes: false, env })

type InitialLoadEvent = { tag: 'InitialLoad' }
type ScrollEvent = { tag: 'Scroll', direction: 'Up' | 'Down', entries: Array<IntersectionObserverEntry>}
type SemanticEvent = InitialLoadEvent | ScrollEvent

type ScrollDownAction =
  | { tag: 'SetBreadcrumb', target: HTMLElement }
  | { tag: 'DoNothing' }
type ScrollUpAction =
  | { tag: 'SetBreadcrumb', target: HTMLElement }
  | { tag: 'DoNothing' }
  | { tag: 'DeleteBreadcrumbs' }
type InitialLoadAction =
  | { tag: 'SetBreadcrumb', target: HTMLElement }
  | { tag: 'DoNothing' }
type Action =
  | { tag: 'SetBreadcrumb', target: HTMLElement }
  | { tag: 'DoNothing' }
  | { tag: 'DeleteBreadcrumbs' }

// Interpret raw input

let lastScrollPosition

const interpretEvent = (targets: Array<HTMLElement>) => (entries: Array<IntersectionObserverEntry>): SemanticEvent => {
  const isInitialLoad = lastScrollPosition === undefined
  if (isInitialLoad) {
    lastScrollPosition = window.scrollY
    return { tag: 'InitialLoad', entries }
  } else {
    const direction = window.scrollY >= lastScrollPosition ? 'Down' : 'Up'
    lastScrollPosition = window.scrollY
    return { tag: 'Scroll', direction, entries }
  }
}

// Core logic

const determineActionOnScrollDown: (ScrollEvent => ScrollDownAction) = S.pipe([
  evt => evt.entries,
  S.filter(entry => !entry.isIntersecting),
  S.last,
  S.maybe({ tag: 'DoNothing' })(entry => {
    return { tag: 'SetBreadcrumb', target: entry.target }
  })
])

const determineActionOnScrollUp = (targets: Array<HTMLElement>): (ScrollEvent => ScrollUpAction) =>
  S.pipe([
    evt => evt.entries,
    S.filter(entry => entry.isIntersecting),
    S.head,
    S.map(entry => entry.target),
    S.map(target => targets.findIndex(t => t === target)),
    S.maybe({ tag: 'DoNothing' })(
      index =>
        index === 0
          ? { tag: 'DeleteBreadcrumbs' }
          : { tag: 'SetBreadcrumb', target: targets[index - 1] }
    )
  ])

const determineActionOnInitialLoad = (targets: Array<HTMLElement>) => (event: InitialLoadEvent): InitialLoadAction =>
  S.pipe([
    S.filter(target => target.getBoundingClientRect().top < 0),
    S.last,
    S.maybe({ tag: 'DoNothing' })(target => {
      return { tag: 'SetBreadcrumb', target }
    })
  ])(targets)

const determineAction = (targets: Array<HTMLElement>) => (event: SemanticEvent): Action => {
  switch (event.tag) {
    case 'InitialLoad':
      return determineActionOnInitialLoad(targets)(event)
    case 'Scroll':
      switch (event.direction) {
        case 'Up':
          return determineActionOnScrollUp(targets)(event)
        case 'Down':
          return determineActionOnScrollDown(event)
        default:
          ;(event.direction.tag: empty) // eslint-disable-line no-unused-expressions
          throw new Error(event.toString())
      }
    default:
      ;(event.direction: empty) // eslint-disable-line no-unused-expressions
      throw new Error(event.toString())
  }
}

// Actions

const executeAction = (targets: Array<HTMLElement>) => (action: Action): void => {
  switch (action.tag) {
    case 'SetBreadcrumb':
      getOrCreateBreadcrumbs().innerHTML = prettyPrintHeaders(
        breadcrumbHeaders(targets)(action.target)
      )
      break
    case 'DeleteBreadcrumbs':
      deleteBreadcrumbs()
      break
    case 'DoNothing':
      break
  }
}

const deleteBreadcrumbs = (): void => {
  S.pipe([
    toMaybe,
    S.map(bc => {
      bc.outerHTML = ''
    })
  ])(document.getElementById('article-breadcrumb'))
}

const getOrCreateBreadcrumbs = (): HTMLElement =>
  S.pipe([toMaybe, S.fromMaybe_(createBreadcrumbs)])(
    document.querySelector('#article-breadcrumb')
  )

const createBreadcrumbs = (): HTMLElement => {
  const div = document.createElement('div')
  div.id = 'article-breadcrumb'
  S.pipe([
    toMaybe,
    S.fromMaybe_(() => {
      throw new Error('<article> must exist')
    }),
    article => article.appendChild(div)
  ])(document.querySelector('article'))
  return div
}

// Misc

const prettyPrintHeaders = S.pipe([
  headers => Array.from(headers.entries()),
  S.sort,
  S.map(([key, value]) => value.innerText),
  values => values.join(' ⊃ ')
])

const breadcrumbHeaders = (targets: Array<HTMLElement>): (HTMLElement => Map<string, HTMLElement>) =>
  S.pipe([
    target => [target, S.takeWhile(t => t !== target)(targets)],
    ([current, prev]) => [
      current,
      S.filter(target => target.tagName < current.tagName)(prev)
    ],
    ([current, prev]) => S.append(current)(prev),
    targets => new Map(S.map(target => [target.tagName, target])(targets))
  ])

// Entry point

documentReadyPromise.then(() => {
  const targets = Array.from(
    document.querySelectorAll('h3:not(#article-subtitle), h4, h5, h6')
  )
  const observer = new IntersectionObserver(cb(targets), { threshold: 0 })
  targets.forEach(el => observer.observe(el))
})

const cb = (targets: Array<HTMLElement>): (Array<IntersectionObserverEntry> => void) => S.pipe([
  interpretEvent(targets),
  determineAction(targets),
  executeAction(targets)
])
