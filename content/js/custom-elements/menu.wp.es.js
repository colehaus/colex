// @flow
/* eslint no-undef: "off" */

import { create, env } from 'sanctuary'

import {
  asHTMLElement,
  documentReadyPromise,
  fromNullableError,
  getBySelector,
  relative,
  removeElement,
  toMaybe
} from 'libs/util'

const S = create({ checkTypes: false, env })

const defaultHandlers = (el: HTMLElement) => {
  if (el.getAttribute('type') === 'radio') {
    const menu = relative(el => el.closest('menu'))(el)
    const text = fromNullableError(`No text for ${el.toString()}`)(el.innerText)
    const item = relative(el => el.querySelector(`[label="${text}"]`))(menu)
    handleEvent({ tag: 'ITEMSELECT', menu, item })
  }
}

const getMenu = (el: HTMLElement) => {
  const menuRef: HTMLElement = relative(el => el.closest('[type="menu"]'))(el)
  return getBySelector('#' + menuRef.dataset.menu)
}

const renderMenuItem = (menuItem: MenuItem): HTMLElement => {
  switch (menuItem.tag) {
    case 'HR':
      return document.createElement('hr')
    case 'MENUITEM': {
      const li = document.createElement('li')
      li.textContent = menuItem.label
      li.setAttribute('type', menuItem.type)
      if (menuItem.active) {
        li.setAttribute('checked', 'checked')
      } else {
        li.removeAttribute('checked')
      }
      return li
    }
    default:
      ;(menuItem.tag: empty) // eslint-disable-line no-unused-expressions
      throw new Error(menuItem.toString())
  }
}

const renderMenu = (menu: Menu): ?HTMLElement => {
  switch (menu.tag) {
    case 'CLOSED':
      return null
    case 'OPEN': {
      const ul = document.createElement('ul')
      ul.classList.add('menu')
      S.map(S.pipe([renderMenuItem, x => ul.appendChild(x)]))(menu.items)
      return ul
    }
    default:
      ;(menu.tag: empty) // eslint-disable-line no-unused-expressions
      throw new Error(menu.toString())
  }
}

type MenuItemType = 'radio' | string

type MenuItem =
  | { tag: 'HR' }
  | { tag: 'MENUITEM', label: string, type: MenuItemType, active: boolean }

type Menu = { tag: 'OPEN', items: Array<MenuItem> } | { tag: 'CLOSED' }

const parseMenuItem = (menuItem: HTMLElement): MenuItem => {
  switch (menuItem.tagName) {
    case 'HR':
      return { tag: 'HR' }
    case 'MENUITEM':
      return {
        tag: 'MENUITEM',
        label: fromNullableError(`Missing label for ${menuItem.toString()}`)(
          menuItem.getAttribute('label')
        ),
        type: fromNullableError(`Missing type for ${menuItem.toString()}`)(
          menuItem.getAttribute('type')
        ),
        active: menuItem.getAttribute('checked') === 'checked'
      }
    default:
      throw new Error(menuItem.toString())
  }
}

const parseMenu = (menu: HTMLElement): Menu =>
  menu.dataset.active === 'true'
    ? { tag: 'CLOSED' }
    : {
      tag: 'OPEN',
      items: S.map(el => parseMenuItem(el))(
        Array.from(menu.querySelectorAll('menuitem, hr'))
      )
    }

type Event =
  | { tag: 'MENUCLICK', menu: HTMLElement }
  | { tag: 'ITEMSELECT', item: HTMLElement, menu: HTMLElement }

const handleEvent = (event: Event) => {
  switch (event.tag) {
    case 'MENUCLICK':
      S.pipe([
        parseMenu,
        renderMenu,
        toMaybe,
        S.maybe_(() => {
          document.querySelectorAll('ul.menu').forEach(removeElement)
          event.menu.dataset.active = 'false'
        })(menuDom => {
          event.menu.appendChild(menuDom)
          event.menu.dataset.active = 'true'
        })
      ])(event.menu)
      break
    case 'ITEMSELECT':
      document.querySelectorAll('ul.menu').forEach(removeElement)
      event.menu.dataset.active = 'false'
      S.map(el => el.removeAttribute('checked'))(
        Array.from(event.menu.children)
      )
      event.item.setAttribute('checked', 'checked')
      break
  }
}

const toggleMenu = (ev: MouseEvent) => {
  ev.stopPropagation()
  // Menu shouldn't popup when clicking on interactive element
  const isInteractive = [
    'TEXTAREA',
    'BUTTON',
    'INPUT',
    'OPTION',
    'SELECT',
    'DETAILS',
    'SUMMARY',
    'A'
  ].some(tg => asHTMLElement(ev.target).nodeName === tg)
  if (isInteractive) {
    return
  }
  const isPopup = el =>
    el.tagName === 'MENU' && el.getAttribute('type') === 'popup'
  const menu = getMenu(asHTMLElement(ev.target))
  if (isPopup(menu)) {
    handleEvent({ tag: 'MENUCLICK', menu })
  }
  return false
}

documentReadyPromise.then(() => {
  const params = new URLSearchParams(location.search)
  document.querySelectorAll('menu').forEach(menu => {
    const label = params.get(menu.id)
    if (label != null) {
      const item = relative(el => el.querySelector(`[label="${label}"]`))(menu)
      handleEvent({ tag: 'ITEMSELECT', menu, item })
    }
  })

  document.querySelectorAll('[type="menu"]').forEach(el => {
    el.addEventListener('click', toggleMenu)
  })
})

export default { getMenu, defaultHandlers }
