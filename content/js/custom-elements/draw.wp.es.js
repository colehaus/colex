// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import * as d3 from 'd3'
import kernel from 'kernel-smooth'
import { create, env } from 'sanctuary'
const S = create({checkTypes: false, env})

const setHandlers = (_, drawEl) => {
  let points = []
  d3.select(drawEl).select('svg').call(draw(points))
  $(drawEl).children('button').click(clear(points))
}

const clear = points => evt => {
  points.length = 0
  const svg = $(evt.currentTarget).parent().children('svg')[0]
  d3.select(svg).selectAll('path').remove()
}

const interpolate = (selection) => (points) => {
  selection
    .select('.interpolated')
    .remove()
  const func = kernel.regression(S.map(p => p[0])(points), S.map(p => p[1])(points), kernel.fun.gaussian, 10)
  const interpolatedPoints = S.map(x => [x, func(x)])(S.range(0, selection.node().getBoundingClientRect().width))
  selection
    .append('path')
    .attr('d', d3.line()(interpolatedPoints))
    .classed('interpolated', true)
}

const draw = function (points) { return function (selection) {
  let down = false
  let path
  let currentPoints
  selection
    .on('mousedown', function() {
      down = true
      const point = d3.mouse(this)
      currentPoints = [point]
      path = selection
        .append('path')
        .attr('d', d3.line()([point, point]))
        .classed('draw-input', true)
    })
    .on('mouseup', function(){
      down = false
      points.push(currentPoints)
      interpolate(selection)(S.join(points))
    })
    .on('mousemove', function(){
      if (down) {
        currentPoints.push(d3.mouse(this))
        path.attr('d', d3.line()(currentPoints))
      }
    })
}}

$('.draw').each(setHandlers)

