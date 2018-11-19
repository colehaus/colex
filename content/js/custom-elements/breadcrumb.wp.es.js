// @flow
/* eslint no-undef: "off" */

import { create, env } from 'sanctuary'
const S = create({ checkTypes: false, env })

// Handles scrolling down
const lastScrollOff =
  S.pipe([
    S.filter(entry => !entry.isIntersecting),
    S.last,
    S.map(entry => entry.target)
  ])

// Handles scrolling up
const firstScrollBack = targets =>
  S.pipe([
    S.filter(entry => entry.isIntersecting),
    S.head,
    S.map(entry => entry.target),
    S.map(target => targets.findIndex(t => t === target)),
    S.chain(index => index === 0 ? S.Nothing : S.Just(targets[index - 1]))
  ])

const breadcrumbHeaders = targets =>
  S.pipe([
    target => [target, S.takeWhile(t => t !== target)(targets)],
    ([current, prev]) => [current, S.filter(target => target.tagName < current.tagName)(prev)],
    ([current, prev]) => S.append(current)(prev),
    targets => new Map(S.map(target => [target.tagName, target])(targets))
  ])

const cb = targets => entries => {
  // Only look at events about top of viewport
  if (entries.some(entry => entry.boundingClientRect.top < 0)) {
    S.pipe([
      entries =>
        S.justs([
          lastScrollOff(entries),
          firstScrollBack(targets)(entries)
        ]),
      S.head,
      S.map(breadcrumbHeaders(targets)),
      // `Nothing` corresponds to scrolling back past first header, so delete breadcrumbs
      S.maybe_(deleteBreadcrumbs)(headers => {
        getOrCreateBreadcrumbs().innerHTML = prettyPrintHeaders(headers)
      })
    ])(entries)
  }
}

document.addEventListener('DOMContentLoaded', () => {
  if (document.location.pathname.startsWith('/posts/')) {
    const targets = Array.from(document.querySelectorAll('h3:not(#article-subtitle), h4, h5, h6'))
    const observer = new IntersectionObserver(cb(targets), { threshold: 0 })
    targets.forEach(el => observer.observe(el))
  }
})

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
