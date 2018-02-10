// @flow
/* eslint no-undef: "off" */

const makeAnimationPromise = (duration: number, f: (number => *)): Promise<*> =>
  new Promise((resolve, reject) => {
    let start, prog
    const f_ = timestamp => {
      if (start == null) { start = timestamp }
      prog = (timestamp - start) / duration
      if (prog <= 1) {
        f(prog)
        requestAnimationFrame(f_)
      } else {
        resolve()
      }
    }
    requestAnimationFrame(f_)
  })

const uniquify = <A>(as: Array<A>): Array<A> => Array.from((new Set(as)).values())

export { makeAnimationPromise, uniquify }
