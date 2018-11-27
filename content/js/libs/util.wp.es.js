// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import { create, env } from 'sanctuary'
const S = create({ checkTypes: false, env })

// For when we know that a thing is provably not null but it's not apparent to flow
const claimNotNull = <A>(a: ?A): A => (a: any)

const documentReadyPromise: Promise<*> = new Promise((resolve, reject) => $(() => resolve()))

const makeSleepPromise = (milliseconds: number): Promise<*> =>
  new Promise((resolve, reject) => setTimeout(resolve, milliseconds))

const makeAnimationPromise = (milliseconds: number, f: (number => *)): Promise<*> =>
  new Promise((resolve, reject) => {
    let start, prog
    const f_ = timestamp => {
      if (start == null) { start = timestamp }
      prog = (timestamp - start) / milliseconds
      if (prog <= 1) {
        f(prog)
        requestAnimationFrame(f_)
      } else {
        f(1)
        resolve()
      }
    }
    requestAnimationFrame(f_)
  })

const parseMatrixToNumbers = (transformationMatrix: string): Array<number> =>
  S.map(
    S.pipe([
      x => x.trim(),
      Number.parseFloat
    ])
  )(
    transformationMatrix.split('(')[1].split(')')[0].split(',')
  )

const parseMatrixToAngle = (transformationMatrix: string): number =>
  // See https://css-tricks.com/get-value-of-css-rotation-through-javascript/
  Math.round(Math.asin(parseMatrixToNumbers(transformationMatrix)[1]) * (180 / Math.PI))

const parseMatrixToYTranslation = (transformationMatrix: string): number =>
  parseMatrixToNumbers(transformationMatrix)[5]

const uniquify = <A>(as: Array<A>): Array<A> => Array.from((new Set(as)).values())
const uniquifyValue = <A>(as: Array<A>): Array<A> =>
  S.pipe([
    S.map(JSON.stringify),
    uniquify,
    S.map(JSON.parse)
  ])(as)

const unexpectedCase = (impossible: empty): void => {
  throw new Error(`Unexpected case ${impossible}`)
}

export {
  claimNotNull,
  documentReadyPromise,
  makeAnimationPromise,
  makeSleepPromise,
  parseMatrixToAngle,
  parseMatrixToYTranslation,
  uniquify,
  uniquifyValue,
  unexpectedCase
}
