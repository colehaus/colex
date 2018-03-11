// @flow

import {create, env} from 'sanctuary'

const S = create({checkTypes: false, env})

const toSvgPolygon =
  (angles: Array<number>) =>
    (RADIUS: number): string =>
      S.pipe([
        S.map(angle => Math.cos(angle) * RADIUS + ' ' + Math.sin(angle) * RADIUS),
        S.joinWith(', ')
      ])(
        angles
      )

const triangle = [
  3 / 6 * Math.PI,
  7 / 6 * Math.PI,
  11 / 6 * Math.PI
]
const square = [
  1 / 4 * Math.PI,
  3 / 4 * Math.PI,
  5 / 4 * Math.PI,
  7 / 4 * Math.PI
]
const diamond = [
  0 / 2 * Math.PI,
  1 / 2 * Math.PI,
  2 / 2 * Math.PI,
  3 / 2 * Math.PI
]
const pentagon = [
  3 / 10 * Math.PI,
  7 / 10 * Math.PI,
  11 / 10 * Math.PI,
  15 / 10 * Math.PI,
  19 / 10 * Math.PI
]
const hexagon = [
  1 / 6 * Math.PI,
  3 / 6 * Math.PI,
  5 / 6 * Math.PI,
  7 / 6 * Math.PI,
  9 / 6 * Math.PI,
  11 / 6 * Math.PI
]
const circle = S.map(i => 2 * Math.PI / 16 * i)(S.range(0, 16))

export { toSvgPolygon, triangle, square, diamond, pentagon, hexagon, circle }
