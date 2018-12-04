// @flow
/* eslint no-undef: "off" */

import * as d3 from 'd3'
import kernel from 'kernel-smooth'
import { create, env } from 'sanctuary'

import { asHTMLElement } from 'libs/util'

const S = create({ checkTypes: false, env })

export type Box = {
  width: number,
  left: number,
  top: number,
  height: number
}

const setHandlers = (drawEl: HTMLElement, box: Box, callback: ?((number => number)) => void) => {
  let points = []
  d3.select(drawEl).select('svg').call(draw(points, box, callback))
  Array.from(drawEl.children).filter(el => el.matches('button')).forEach(el =>
    el.addEventListener('click', clear(points))
  )
}

const clear = (points: Array<Array<[number, number]>>) => (evt: Event) => {
  points.length = 0
  S.pipe([
    el => el.closest('.draw'),
    S.toMaybe,
    S.chain(
      S.pipe([
        el => el.querySelector('svg'),
        S.toMaybe
      ])),
    S.map(svg => d3.select(svg).selectAll('path').remove())
  ])(asHTMLElement(evt.currentTarget))
}

// `Box` allows us to specify some zone that specifies the domain and range (in pixels) of our interpolated function. i.e. Drawing at the lower left corner of `Box` is (0, 0) instead of some coordinate relative to the SVG origin.
const interpolate = (selection: SelectWithoutData, box: Box, callback: ?((number => number) => void)) => (points: Array<[number, number]>) => {
  selection
    .selectAll('path:not(.draw-input)')
    .remove()

  const scaleX = d3.scaleLinear().domain([box.left, box.left + box.width]).range([0, box.width])
  const scaleY = d3.scaleLinear().domain([box.height + box.top, box.top]).range([0, box.height])
  const func = kernel.regression(S.map(p => scaleX(p[0]))(points), S.map(p => scaleY(p[1]))(points), kernel.fun.gaussian, 10)
  const interpolatedPoints = S.map(x => [scaleX.invert(x), scaleY.invert(func(x))])(S.range(0)(box.width))

  if (interpolatedPoints.every(([x, y]) => !Number.isNaN(x) && !Number.isNaN(y))) {
    if (callback != null) {
      callback(func)
    }
    selection
      .append('path')
      .attr('d', d3.line()(interpolatedPoints))
      .classed('interpolated', true)
  }
}

const draw = function (points: Array<Array<[number, number]>>, box: Box, callback: ?((number => number) => void)) {
  return function (selection: SelectWithoutData) {
    let down = false
    let path
    let currentPoints
    selection
      .on('mousedown', function () {
        down = true
        const point = d3.mouse(this)
        currentPoints = [point]
        path = selection
          .append('path')
          .attr('d', d3.line()([point, point]))
          .classed('draw-input', true)
      })
      .on('mouseup', function () {
        down = false
        points.push(currentPoints)
        interpolate(selection, box, callback)(S.join(points))
      })
      .on('mousemove', function () {
        if (down) {
          currentPoints.push(d3.mouse(this))
          path.attr('d', d3.line()(currentPoints))
        }
      })
  }
}

export { setHandlers }
