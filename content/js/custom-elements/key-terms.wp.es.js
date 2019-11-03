// @flow
/* eslint no-undef: "off" */

import { documentReadyPromise, getBySelector } from 'libs/util'

const close = () => {
  getBySelector('#underlay').classList.remove('inactive')
  const overlay = getBySelector('#overlay')
  overlay.classList.add('inactive')
  overlay.style.overflowY = 'unset'
  history.replaceState(null, '', window.location.pathname)
}

const open = () => {
  getBySelector('#underlay').classList.add('inactive')
  const overlay = getBySelector('#overlay')
  overlay.classList.remove('inactive')
  overlay.addEventListener('click', close)
  // We don't want to just add it to the CSS because it messes up other things
  // that use the overlay (e.g. arg-map)
  overlay.style.overflowY = 'scroll'
}

documentReadyPromise.then(() => {
  if (location.hash.endsWith('key-terms')) {
    open()
  }
  window.addEventListener(
    'hashchange',
    () => {
      if (location.hash.endsWith('key-terms')) {
        open()
      }
    },
    false
  )
})
