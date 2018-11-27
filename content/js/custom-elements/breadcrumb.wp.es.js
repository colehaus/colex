// @flow
/* eslint no-undef: "off" */

import { create, env } from 'sanctuary'

import { unexpectedCase } from 'libs/util'

const S = create({ checkTypes: false, env })

type ScrollDownAction
  = { tag: 'SetBreadcrumb', target: HTMLElement }
  | { tag: 'DoNothing' }

type ScrollUpAction
  = { tag: 'SetBreadcrumb', target: HTMLElement }
  | { tag: 'DoNothing' }
  | { tag: 'DeleteBreadcrumbs' }

type ScrollAction
  = { tag: 'SetBreadcrumb', target: HTMLElement }
  | { tag: 'DoNothing' }
  | { tag: 'DeleteBreadcrumbs' }

// Handles scrolling down
const lastScrollOff =
  S.pipe([
    S.filter(entry => !entry.isIntersecting && scrollDirection === 'down'),
    S.last,
    S.maybe({ tag: 'DoNothing' })(entry => { return { tag: 'SetBreadcrumb', target: entry.target } })
  ])

// Handles scrolling up
const firstScrollBack = targets =>
  S.pipe([
    S.filter(entry => entry.isIntersecting && scrollDirection === 'up'),
    S.head,
    S.map(entry => entry.target),
    S.map(target => targets.findIndex(t => t === target)),
    S.maybe({ tag: 'DoNothing' })(index => index === 0 ? { tag: 'DeleteBreadcrumbs' } : { tag: 'SetBreadcrumb', target: targets[index - 1] })
  ])

const breadcrumbHeaders = targets =>
  S.pipe([
    target => [target, S.takeWhile(t => t !== target)(targets)],
    ([current, prev]) => [current, S.filter(target => target.tagName < current.tagName)(prev)],
    ([current, prev]) => S.append(current)(prev),
    targets => new Map(S.map(target => [target.tagName, target])(targets))
  ])

const combineActions = (scrollDown: ScrollDownAction, scrollUp: ScrollUpAction): ScrollAction => {
  // Humor `flow` for exhaustiveness checking
  const downTag = scrollDown.tag
  switch (downTag) {
    case 'SetBreadcrumb':
      const upTag = scrollUp.tag
      switch (upTag) {
        case 'SetBreadcrumb':
        case 'DeleteBreadcrumbs':
          throw new Error('Somehow scrolling up and down simultaneously')
        case 'DoNothing':
          return scrollDown
        default:
          unexpectedCase(upTag)
          return (null: any)
      }
    case 'DoNothing':
      return scrollUp
    default:
      unexpectedCase(downTag)
      return (null: any)
  }
}

const executeAction = (targets) => (action: ScrollAction): void => {
  switch (action.tag) {
    case 'SetBreadcrumb':
      getOrCreateBreadcrumbs().innerHTML = prettyPrintHeaders(breadcrumbHeaders(targets)(action.target))
      break
    case 'DeleteBreadcrumbs':
      deleteBreadcrumbs()
      break
    case 'DoNothing':
      break
  }
}

const cb = targets => entries => {
  scrollDirection = window.scrollY >= lastScrollPosition ? 'down' : 'up'
  lastScrollPosition = window.scrollY
  executeAction(targets)(combineActions(lastScrollOff(entries), firstScrollBack(targets)(entries)))
}

var lastScrollPosition, scrollDirection

if (document.location.pathname.startsWith('/posts/')) {
  document.addEventListener('DOMContentLoaded', () => {
    lastScrollPosition = window.scrollY
    const targets = Array.from(document.querySelectorAll('h3:not(#article-subtitle), h4, h5, h6'))
    const observer = new IntersectionObserver(cb(targets), { threshold: 0 })
    targets.forEach(el => observer.observe(el))
  })
}

const prettyPrintHeaders =
  S.pipe([
    headers => Array.from(headers.entries()),
    S.sort,
    S.map(([key, value]) => value.innerText),
    values => values.join(' ⊃ ')
  ])

const deleteBreadcrumbs = (): void => {
  S.pipe([
    S.toMaybe,
    S.map(bc => {
      bc.outerHTML = ''
    })
  ])(document.getElementById('article-breadcrumb'))
}

const getOrCreateBreadcrumbs = (): HTMLElement =>
  S.pipe([
    S.toMaybe,
    S.fromMaybe_(createBreadcrumbs)
  ])(document.querySelector('#article-breadcrumb'))

const createBreadcrumbs = (): HTMLElement => {
  const div = document.createElement('div')
  div.id = 'article-breadcrumb'
  S.pipe([
    S.toMaybe,
    S.fromMaybe_(() => { throw new Error('<article> must exist') }),
    article => article.appendChild(div)
  ])(document.querySelector('article'))
  return div
}
