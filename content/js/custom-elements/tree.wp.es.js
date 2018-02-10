// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import {create, env} from 'sanctuary'

import menu from 'custom-elements/menu'
import sidenote from 'custom-elements/sidenote'
import { makeAnimationPromise } from 'libs/util'

const S = create({checkTypes: false, env})

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
      makeAnimationPromise(300, prog => event_.from.css('opacity', 1 - prog))
        .then(() => {
          const pho = event_.parent.height()
          event_.from.css('opacity', 0)
          event_.to.css('opacity', 0)
          event_.from.removeClass('open')
          event_.to.addClass('open')
          const phn = event_.parent.height()
          return [pho, phn]
        }).then(([pho, phn]) =>
          makeAnimationPromise(300, prog => {
            event_.to.css('opacity', prog)
            event_.parent.css('height', pho + (phn - pho) * prog)
          })
        ).then(() => {
          event_.parent.removeAttr('style')
          event_.from.removeAttr('style')
          event_.to.removeAttr('style')
        })
        .then(resolve)
      break
  }
}

const choose = (ev: JQueryEventObject) => {
  ev.stopPropagation()
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
}

const fixTarget = (target: EventTarget) => HTMLElement = (target: any) // eslint-disable-line no-return-assign

$(() => {
  S.map(el =>
    $(el).click(({pageY, pageX, target}) =>
      menu.getMenu(fixTarget(target))
      .offset({top: pageY, left: pageX})
      .children('ul.menu').children()
      .off().click(choose))
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

// const x: Array<string> = ["ab", "bc"]
// const y: Array<number> = S.map(q => q.length)(x)
// const z: Maybe<number> = S.toMaybe(1)
// const w: Maybe<number> = S.map(q => q)(z)
