// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import {create, env} from 'sanctuary'
const S = create({checkTypes: false, env})

const defaultHandlers = (el: JQuery) => {
  if (el.attr('type') === 'radio') {
    const menu = el.closest('menu')
    const item = menu.find(`[label="${el.text()}"]`)
    handleEvent({ tag: 'ITEMSELECT', menu, item })
  }
}

const getMenu = (el: HTMLElement) => $('#' + $(el).closest('[type="menu"]').attr('data-menu'))

const renderMenuItem = (menuItem: MenuItem): JQuery => {
  switch (menuItem.tag) {
    case 'HR':
      return $('<hr/>')
    case 'MENUITEM':
      const li = $('<li/>').text(menuItem.label).attr('type', menuItem.type)
      if (menuItem.active) {
        li.attr('checked', 'checked')
      } else {
        li.removeAttr('checked')
      }
      return li
    default:
      throw Error(menuItem.toString())
  }
}

const renderMenu = (menu: Menu): ?JQuery => {
  switch (menu.tag) {
    case 'CLOSED':
      return null
    case 'OPEN':
      return $('<ul class="menu"></ul>').append(S.map(renderMenuItem)(menu.items))
  }
}

type MenuItemType = 'radio' | string

type MenuItem
  = { tag: 'HR' }
  | { tag: 'MENUITEM', label: string, type: MenuItemType, active: boolean }

type Menu
  = { tag: 'OPEN', items: Array<MenuItem> }
  | { tag: 'CLOSED' }

// The JQuery declaration seems to be wrong
const fixTarget = (target: EventTarget) => HTMLElement = (target: any) // eslint-disable-line no-return-assign

const parseMenuItem = (menuItem: JQuery): MenuItem => {
  switch (menuItem.get(0).nodeName) {
    case 'HR':
      return { tag: 'HR' }
    case 'MENUITEM':
      return { tag: 'MENUITEM',
        label: menuItem.attr('label'),
        type: menuItem.attr('type'),
        active: menuItem.attr('checked') === 'checked'
      }
    default:
      throw Error(menuItem.toString())
  }
}

const parseMenu = (menu: JQuery): Menu =>
  menu.data('active')
  ? { tag: 'CLOSED' }
  : { tag: 'OPEN',
    items: S.map((el) => parseMenuItem($(el)))(menu.children('menuitem, hr').toArray())
  }

type Event
  = { tag: 'MENUCLICK', menu: JQuery }
  | { tag: 'ITEMSELECT', item: JQuery, menu: JQuery }

const handleEvent = (event: Event) => {
  switch (event.tag) {
    case 'MENUCLICK':
      S.pipe([
        parseMenu,
        renderMenu,
        S.toMaybe,
        S.maybe_(
          () => {
            $('ul.menu').remove()
            event.menu.data('active', false)
          })(
          menuDom => {
            event.menu.append(menuDom)
            event.menu.data('active', true)
          }
        )
      ])(
        event.menu
      )
      break
    case 'ITEMSELECT':
      $('ul.menu').remove()
      event.menu.data('active', false)
      S.map(el => $(el).removeAttr('checked'))(event.menu.children().toArray())
      event.item.attr('checked', 'checked')
      break
  }
}

const toggleMenu = (ev: JQueryEventObject) => {
  // Menu shouldn't popup when clicking on interactive element
  const isInter = [
    'TEXTAREA',
    'BUTTON',
    'INPUT',
    'OPTION',
    'SELECT',
    'DETAILS',
    'SUMMARY',
    'A'
  ].some(tg => fixTarget(ev.target).nodeName === tg)
  if (isInter) { return }
  const isPopup = el =>
    $(el).get(0).nodeName === 'MENU' && $(el).attr('type') === 'popup'
  const menu = getMenu(fixTarget(ev.target)).filter((_, el) => isPopup(el))
  if (menu != null) {
    handleEvent({ tag: 'MENUCLICK', menu: $(menu) })
  }
}

$(() => {
  $('[type="menu"]').each((_, el) => {
    $(el).click(toggleMenu)
  })
})

export default {getMenu, defaultHandlers}
