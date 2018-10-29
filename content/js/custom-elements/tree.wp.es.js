// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import { create, env } from 'sanctuary'
import { interpolateNumber } from 'd3'

import menu from 'custom-elements/menu'
import sidenote from 'custom-elements/sidenote'
import { makeAnimationPromise, documentReadyPromise } from 'libs/util'

const S = create({ checkTypes: false, env })

type Event
  = { tag: 'INVISIBLEPARENT', from: JQuery, to: JQuery }
  | { tag: 'SELECTEDOPEN' }
  | { tag: 'SELECTEDNEWVISIBLE', from: JQuery, to: JQuery, parent: JQuery }

const handleEvent = (event: Event) => (resolve: Function, reject: Function) => {
  switch (event.tag) {
    case 'INVISIBLEPARENT':
      event.from.removeClass('open')
      event.to.addClass('open')
      resolve()
      break
    case 'SELECTEDOPEN':
      resolve()
      break
    case 'SELECTEDNEWVISIBLE':
      // Flow behaving badly
      const event_ = event
      const pho = event_.parent.height()
      const stepDuration = 300 // ms
      const fadeOut = () =>
        makeAnimationPromise(stepDuration, prog => event_.from.css('opacity', 1 - prog))
          .then(() => {
            event_.from.css('opacity', 0)
            event_.from.removeClass('open')
          })
      const stretch = () =>
        Promise.resolve()
          .then(() => {
            const heightFn = interpolateNumber(pho, event_.parent.height())
            return makeAnimationPromise(stepDuration, prog => {
              event_.parent.css('height', heightFn(prog))
            })
          })
      const fadeIn = () =>
        Promise.resolve()
          .then(() => {
            event_.to.css('opacity', 0)
            event_.to.addClass('open')
            return makeAnimationPromise(stepDuration, prog => {
              event_.to.css('opacity', prog)
            })
          })
      const cleanUp = () => {
        event_.parent.removeAttr('style')
        event_.from.removeAttr('style')
        event_.to.removeAttr('style')
      }
      fadeOut()
        .then(() => Promise.all([fadeIn(), stretch()]))
        .then(cleanUp)
        .then(resolve)
      break
  }
}

const choose = (ev: JQueryMouseEventObject) => {
  const el = $(ev.target)
  S.map(
    contentTree => {
      const contentBranch = $($(contentTree).children().get(el.index()))
      const event =
        contentBranch.hasClass('open')
          ? { tag: 'SELECTEDOPEN' }
          : contentBranch.parent().is(':visible')
            ? { tag: 'SELECTEDNEWVISIBLE', from: contentBranch.siblings('.open'), to: contentBranch, parent: contentBranch.parent() }
            : { tag: 'INVISIBLEPARENT', from: contentBranch.siblings('.open'), to: contentBranch }
      new Promise(handleEvent(event)).then(() => {
        sidenote.setNotes()
        sidenote.fixNotes()
      })
    }
  )(
    $(`[data-menu="${el.closest('menu').attr('id')}"]`).toArray()
  )
  menu.defaultHandlers(el)
  return false
}

const fixTarget = (target: EventTarget) => HTMLElement = (target: any) // eslint-disable-line no-return-assign

documentReadyPromise.then(() => {
  const params = new URLSearchParams(location.search)
  $('[type="menu"]').each((_, menu_) => {
    const menu = $(menu_)
    const id = menu.data('menu')
    const label = params.get(id)
    if (label != null) {
      const menuItem = $('#' + id).children(`[label="${label}"]`)
      const from = menu.children('.open')
      const to = $(menu.children().get(menuItem.index()))
      new Promise(handleEvent({ tag: 'INVISIBLEPARENT', from, to })).then(() => {
        sidenote.setNotes()
        sidenote.fixNotes()
      })
    }
  })

  S.map(el =>
    $(el).click(({ pageY, pageX, target }) => {
      menu.getMenu(fixTarget(target))
        .offset({ top: pageY, left: pageX })
        .children('ul.menu').children()
        .off().click(choose)
      return false
    })
  )(
    $('[type="menu"]').toArray()
  )

  MathJax.Hub.Queue(() => {
    // Messes up rendering if we add to stylesheet
    $('.MathJax_MathContainer').css('display', 'inline')
    $('.MathJax_MathContainer > span').css('display', 'inline')
    // Re-inline fix rendering problem
    const inlines = $('.switch.inline > li.open')
    inlines.css('display', 'inline-block')
    inlines.offset() // Trigger reflow
    inlines.css('display', 'inline')
    inlines.removeAttr('style')
  })
})
