// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import * as d3 from 'd3'
import {create, env} from 'sanctuary'

import { uniquify } from 'libs/util'
import * as shapesImport from 'libs/shapes'

const S = create({checkTypes: false, env})

const linkArc = ({target, source}) => {
  const dx = target.x - source.x
  const dy = target.y - source.y
  const dr = Math.hypot(dx, dy)
  return `M${source.x},${source.y}A${dr},${dr} 0 0,1 ${target.x},${target.y}`
}

export type NodeText<Ty> = {| label: Array<string>, type: Ty |}
type NodeLayoutConfig = {| x: number => number, y: number => number |}
// Flow is kinda dumb so we can't construct these out of the intersections
export type NodeConfigWithLayout<Ty> = {| label: Array<string>, type: Ty, x: number => number, y: number => number |}
type NodeConfig<Ty> = NodeConfigWithLayout<Ty> | NodeText<Ty>
type NodeWithLayout<Ty> = {| label: Array<string>, type: Ty, x: number, y: number, url: string |}
type NodeWithoutLayout<Ty> = {| label: Array<string>, type: Ty, url: string |}
type Node<Ty> = NodeWithLayout<Ty> | NodeWithoutLayout<Ty>
type Link<Ty> = { source: string, target: string, type: Ty }
type LinkN<LTy, NTy> = { source: Node<NTy>, target: Node<NTy>, type: LTy }
type NodeType<Ty> = { type: Ty, label: Array<string>, shape: number => string }
type LinkType<Ty> = { type: Ty, label: Array<string> }
type D3 = any
type CanvasDimensions = { width: number, height: number }

const RADIUS = 13
const DEFAULT_STRENGTH = 0.2

const mkLegend = <NTy, LTy>(canvasId: string, svg: D3, nodeTypeData: Array<NodeType<NTy>>, linkTypeData: Array<LinkType<LTy>>) => {
  svg.append('g')
    .attr('transform', 'translate(20, 50)')
    .classed('legend', true)
    .selectAll('.legend')
    .data(nodeTypeData)
    .enter().append('g')
      .attr('transform', (_, i) => 'translate(0, ' + i * 30 + ')')
      .each(function (d) {
        const nodeType = nodeTypeData.find(t => t.type === d.type)
        if (nodeType == null) {
          throw Error(`Couldn't find node type ${d.type} in ${nodeTypeData.toString()}`)
        }
        d3.select(this).append('polygon').attr('points', nodeType.shape(RADIUS))
        d.label = d.label || []
        const text = d3.select(this).append('text')
          .attr('x', RADIUS + 5)
        d.label.forEach((t, i) => {
          text.append('tspan')
            .attr('y', 0.5 + 1.6 * i + 'ex')
            .classed('node', true)
            .text(t)
        })
      })

  svg.append('g')
    .attr('transform', 'translate(20, 180)')
    .classed('legend', true)
    .selectAll('.legend')
    .data(linkTypeData)
    .enter().append('g')
      .attr('transform', (_, i) => 'translate(0, ' + i * 30 + ')')
      .each(function (d) {
        d3.select(this).append('path')
          .attr('d', linkArc({source: {x: -RADIUS, y: RADIUS},
            target: {x: RADIUS, y: -RADIUS}}))
          .classed(d.type, true)
          .classed('link', true)
        d.label = d.label || []
        const text = d3.select(this).append('text')
          .attr('x', RADIUS + 5)
        d.label.forEach((t, i) => {
          text.append('tspan')
            .attr('y', 0.5 + 1.6 * i + 'ex')
            .classed('node', true)
            .text(t)
        })
      })
}

const mkForce = <NTy, LTy>(canvas: CanvasDimensions, svg: D3, nodeData: { [string] : Node<NTy> }, linkData: Array<LinkN<LTy, NTy>>, nodeTypeData: Array<NodeType<NTy>>) => {
  const defaultLength = canvas.width / 5

  const tick = () => {
    const transform = ({x, y}) => `translate(${x},${y})`

    const linkCenter = ({target, source}) => {
      const xAvg = (target.x + source.x) / 2
      const yAvg = (target.y + source.y) / 2
      return `translate(${xAvg},${yAvg})`
    }

    links.attr('d', linkArc)
    nodes.attr('transform', transform)
    nodeTexts.attr('transform', transform)
    linkTexts.attr('transform', linkCenter)
  }

  const force = d3.layout.force()
    .nodes(d3.values(nodeData))
    .links(linkData)
    .size([canvas.width, canvas.height])
    .linkDistance(({length}) => length || defaultLength)
    .linkStrength(({strength}) => strength || DEFAULT_STRENGTH)
    .gravity(0.05)
    .charge(-300)
    .on('tick', tick)

  const linkTypes = uniquify(linkData.map(({type}) => type))
  // Per-type markers, as they don't inherit styles.
  svg.append('defs').selectAll('marker')
      .data(linkTypes)
    .enter().append('marker')
      .attr('id', d => d)
      .attr('viewBox', '0 -5 10 10')
      .attr('refX', 15)
      .attr('refY', -1.5)
      .attr('markerWidth', 6)
      .attr('markerHeight', 6)
      .attr('orient', 'auto')
    .append('path')
      .attr('d', 'M0,-5L10,0L0,5')

  const links = svg.append('g').selectAll('path')
      .data(force.links())
    .enter().append('path')
      .attr('class', ({type}) => 'link ' + type)
      .attr('marker-end', ({type}) => 'url(#' + type + ')')

  const linkTexts = svg.append('g').selectAll('text')
      .data(force.links())
    .enter().append('text')
    .each(function (d) {
      d.label = d.label || []
      d.label.forEach((t, i) => {
        d3.select(this).append('tspan')
          .classed('link', true)
          .text(t)
      })
    })

  const nodes = svg.append('g').selectAll('polygon')
      .data(force.nodes())
    .enter().append('polygon')
      .attr('class', ({type}) => type)
      .attr('points', d => {
        const type = nodeTypeData.find(t => t.type === d.type)
        if (type == null) {
          throw Error(`Couldn't find node type ${d.type} in ${nodeTypeData.toString()}`)
        } else {
          return type.shape(RADIUS)
        }
      }).on('dblclick', function (d) {
        d3.select(this).classed('fixed', d.fixed = false)
      }).call(force.drag().on('dragend', function (d) {
        d3.select(this).classed('fixed', d.fixed = true)
      }))

  const nodeTexts = svg.append('g').selectAll('a')
      .data(force.nodes())
    .enter().append('a')
      .attr('xlink:href', ({url}) => url)
    .append('text')
    .each(function (d) {
      d.label = d.label || []
      d.label.forEach((t, i) => {
        d3.select(this).append('tspan')
           .attr('x', 10)
           .attr('y', 1.6 * i + 'ex')
           .classed('node', true)
           .text(t)
      })
    })
  return force
}

const prepData = <LTy, NTy>(canvas: CanvasDimensions, linkData: Array<Link<LTy>>, nodeData: { [string]: NodeConfig<NTy> }): [Array<LinkN<LTy, NTy>>, { [string]: Node<NTy> }] => {
  const xCenter = canvas.width / 2
  const yCenter = canvas.height / 2

  const nodeDataPrepped =
    S.pipe([
      S.keys,
      S.map(key => {
        const node = nodeData[key]
        const { label, type } = nodeData[key]
        if (node.x && node.y) {
          return [key, { url: '#' + key, label, type, x: node.x(canvas.width) + xCenter, y: node.y(canvas.height) + yCenter }]
        } else {
          return [key, { url: '#' + key, label, type }]
        }
      }),
      S.fromPairs
    ])(
      nodeData
    )
  const linkDataPrepped = S.map(link => {
    const { source, target, type } = link
    return { type, source: nodeDataPrepped[source], target: nodeDataPrepped[target] }
  })(
    linkData
  )
  return [linkDataPrepped, nodeDataPrepped]
}

const mkMap = <LTy, NTy>(
  canvasId: string,
  nodeData: { [string]: NodeConfig<NTy> },
  linkData: Array<Link<LTy>>,
  nodeTypeData: Array<NodeType<NTy>>,
  linkTypeData: Array<LinkType<LTy>>) => {
  const canvas = { width: $(canvasId).width(), height: $(canvasId).height() }

  const [linkDataPrepped, nodeDataPrepped] = prepData(canvas, linkData, nodeData)

  const svg = d3.select(canvasId).append('svg')
    .attr('width', canvas.width)
    .attr('height', canvas.height)

  mkLegend(canvasId, svg, nodeTypeData, linkTypeData)
  const force = mkForce(canvas, svg, nodeDataPrepped, linkDataPrepped, nodeTypeData)
  return [force, svg, canvasId]
}

const activate = (map: D3) => (e: ?JQueryEventObject) => {
  const close = () => {
    $('#arg-map a').removeAttr('style')
    $('#underlay').removeClass('inactive')
    $('#overlay').addClass('inactive')
    location.hash = ''
    map.stop()
  }

  let first = true
  $('#underlay').addClass('inactive')
  $('#overlay').removeClass('inactive')
  if (first) {
    map.start()
    $('#arg-map a').click(close)
    $('#arg-map > svg, #overlay').click(function (e) {
      if (e.target === this) { close() }
    })
    first = false
  } else {
    map.resume()
  }
  if (e != null) {
    // SVG requires that we not quote id here?
    const id = $(e.target).attr('id')
    if (id !== undefined) {
      $('g a').filter((_, a) =>
                      // $FlowFixMe Doesn't know about SVG anchors
                      a.href.baseVal === '#' + id
                     ).css('font-weight', 'bold')
    }
  }
}

const handler = (map: D3, svg: D3, canvasId: string) => {
  $(window).resize(() => {
    svg.attr('width', $(canvasId).width())
    svg.attr('height', $(canvasId).height())
  })
  if (location.hash === '#arg-map') {
    activate(map)()
  }
  $('a[href="#arg-map"]').click(activate(map))
}

const shapes = {
  square: shapesImport.toSvgPolygon(shapesImport.square),
  circle: shapesImport.toSvgPolygon(shapesImport.circle),
  diamond: shapesImport.toSvgPolygon(shapesImport.diamond),
  triangle: shapesImport.toSvgPolygon(shapesImport.triangle),
  hexagon: shapesImport.toSvgPolygon(shapesImport.hexagon),
  pentagon: shapesImport.toSvgPolygon(shapesImport.pentagon)
}

export {
  shapes,
  mkMap,
  handler
}
