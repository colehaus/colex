// @flow
/* eslint no-undef: "off" */

import $ from 'jquery'
import * as d3 from 'd3'
import asciiStringSplit from 'ascii-string-split'
import { create, env } from 'sanctuary'

import * as shapesImport from 'libs/shapes'
import { uniquify, uniquifyValue, documentReadyPromise } from 'libs/util'
const S = create({ checkTypes: false, env })

type Shape = 'circle' | 'triangle' | 'diamond' | 'square' | 'pentagon' | 'hexagon'
type Node<Ty: string> = { label: string, type: Ty, id: string }
type NodeType<Ty: string> = { type: Ty, label: string, shape: Shape }
type Link<Ty: string> = { source: string, target: string, type: Ty }
type LinkType<Ty: string> = { type: Ty, label: string }

const RADIUS = 13
const STRENGTH = 1
const LABEL_LENGTH = 20

const shapes = [
  shapesImport.toSvgPolygon(shapesImport.square),
  shapesImport.toSvgPolygon(shapesImport.circle),
  shapesImport.toSvgPolygon(shapesImport.diamond),
  shapesImport.toSvgPolygon(shapesImport.hexagon),
  shapesImport.toSvgPolygon(shapesImport.pentagon),
  shapesImport.toSvgPolygon(shapesImport.triangle)
]

const linkArc = (link: ForceLink) => {
  const { target, source } = link
  const dx = target.x - source.x
  const dy = target.y - source.y
  const dr = Math.hypot(dx, dy)
  return `M${source.x},${source.y}A${dr},${dr} 0 0,1 ${target.x},${target.y}`
}

const fetchData = <NTy: string, LTy: string>(dataSrc: string): Promise<[Array<Node<NTy>>, Array<Link<LTy>>, Array<NodeType<NTy>>, Array<LinkType<LTy>>]> =>
  Promise.all([
    d3.csv(dataSrc + '/nodes.csv'),
    d3.csv(dataSrc + '/links.csv'),
    d3.csv(dataSrc + '/node-types.csv'),
    d3.csv(dataSrc + '/link-types.csv')
  ])

const isUndirectedLink = <LTy: string>(links: Array<Link<LTy>>): (Link<LTy> => boolean) => link =>
  S.pipe([
    S.find(l => l.target === link.source && l.source === link.target),
    S.isJust
  ])(links)
const toUndirectedLink = <LTy: string>(link: Link<LTy>): Link<LTy> => {
  return {
    source: link.source < link.target ? link.source : link.target,
    target: link.source < link.target ? link.target : link.source,
    type: link.type
  }
}
// Unnecessarily quadratic but n is small
const separateDirectedAndUndirected = <LTy: string>(links: Array<Link<LTy>>): [Array<Link<LTy>>, Array<Link<LTy>>] =>
  [
    S.reject(isUndirectedLink(links))(links),
    S.pipe([
      S.filter(isUndirectedLink(links)),
      S.map(toUndirectedLink),
      uniquifyValue
    ])(links)
  ]

// Hacky way of telling Flow about d3's property additions
const forceNodeProps = <T>(t: T): (T & ForceNode) => (t: any)
const forceLinkProps = <T>(t: T): (T & ForceLink) => (t: any)

const ticked = <A: ForceLink, B: ForceNode>(directedLink: SelectWithData<A>, undirectedLink: SelectWithData<A>, node: SelectWithData<B>) => () => {
  directedLink.attr('d', linkArc)
  undirectedLink.attr('d', linkArc)
  node.attr('transform', d => `translate(${d.x}, ${d.y})`)
}

const boundingBox = (width: number, height: number, nodes: Array<Node<*>>): void => {
  const margin = 10
  S.map(node_ => {
    // Tell Flow about the properties d3 has added
    const node: ForceNode = (node_: any)
    if (node.x + node.vx < margin) {
      node.x = margin
      node.vx = 0
    }
    if (node.y + node.vy < margin) {
      node.y = margin
      node.vy = 0
    }
    if (node.x + node.vx > width - margin) {
      node.x = width - margin
      node.vx = 0
    }
    if (node.y + node.vy > height - margin) {
      node.y = height - margin
      node.vy = 0
    }
  })(nodes)
}

const mkSimulation = ({ width, height }, nodes) =>
  d3.forceSimulation()
    .force('link', d3.forceLink().id(d => d.id).strength(STRENGTH).distance(width / 7))
    .force('charge', d3.forceManyBody().strength(-250))
    .force('center', d3.forceCenter(width / 2, height / 2))
    .force('bound', (alpha) => boundingBox(width, height, nodes))
    .alphaDecay(0.015)

const drawLibrary = <Ty: string>(id: string, svg: SelectWithoutData, linkTypes: Array<LinkType<Ty>>) => {
  const uniqueLinkTypes = uniquify(linkTypes.map(d => d.type))
  // Per-type markers, as they don't inherit styles.
  const defs = svg.append('defs')
  defs.append('g').selectAll('marker')
    .data(uniqueLinkTypes)
    .enter().append('marker')
    .attr('id', d => `${id}-marker-${d}`)
    .attr('viewBox', '0 -5 10 10')
    .attr('refX', 15)
    .attr('refY', -1.5)
    .attr('markerWidth', 6)
    .attr('markerHeight', 6)
    .attr('orient', 'auto')
    .append('path')
    .attr('d', 'M0,-5L10,0L0,5')
  defs.append('g').selectAll('linearGradient')
    .data(uniqueLinkTypes)
    .enter().append('linearGradient')
    .attr('id', d => `${id}-gradient-${d}`)
    .append('stop')
}

const drawNode = <Ty: string>(nodeTypes: Array<NodeType<Ty>>): (Ty => string) => {
  return type => {
    const nodeTypeIndex = nodeTypes.findIndex(t => t.type === type)
    if (nodeTypeIndex == null) {
      throw Error(`Couldn't find node type ${type} in ${nodeTypes.toString()}`)
    }
    return shapes[nodeTypeIndex](RADIUS)
  }
}

const drawLinks = <Ty: string>(id: string, svg: SelectWithoutData, links: Array<Link<Ty>>): SelectWithData<Link<Ty> & ForceLink> =>
  svg.append('g')
    .attr('class', 'links')
    .selectAll('path')
    .data(S.map(forceLinkProps)(links))
    .enter().append('path')
    .attr('class', d => d.type)
    .classed('link', true)
    .attr('marker-end', d => `url(#${id}-marker-${d.type})`)
    .attr('stroke', d => `url(#${id}-gradient-${d.type})`)

const drawUndirectedLinks = <Ty: string>(id: string, svg: SelectWithoutData, links: Array<Link<Ty>>): SelectWithData<Link<Ty> & ForceLink> =>
  svg.append('g')
    .attr('class', 'links')
    .selectAll('path')
    .data(S.map(forceLinkProps)(links))
    .enter().append('path')
    .attr('class', d => d.type)
    .classed('link', true)
    .attr('stroke', d => `url(#${id}-gradient-${d.type})`)

const drawNodes = (svg: SelectWithoutData, nodes: Array<Node<*>>, nodeTypes: Array<NodeType<*>>, simulation: Simulation<Node<*>, Link<*>>): SelectWithData<Node<*> & ForceNode> => {
  const node =
    svg.append('g')
      .classed('nodes', true)
      .selectAll('g.node')
      .data(S.map(forceNodeProps)(nodes))
      .enter().append('g')
      .classed('node', true)
      .on('dblclick', dblClicked)
      .call(d3.drag()
        .on('start', dragStarted(simulation))
        .on('drag', dragged)
        .on('end', dragEnded(simulation)))

  node.append('polygon')
    .attr('class', d => d.type)
    .attr('points', d => drawNode(nodeTypes)(d.type))

  drawTSpans(d => d.label, RADIUS + 5)(node.append('a').attr('xlink:href', d => '#' + d.id).append('text'))
  return node
}

const mkArgMap = (id: string, dataSrc: string, canvasSelector: string): Promise<Simulation<Node<*>, Link<*>>> => {
  return fetchData(dataSrc).then(([nodes, links_, nodeTypes, linkTypes]) => {
    const [directedLinks, undirectedLinks] = separateDirectedAndUndirected(links_)
    const links = S.concat(directedLinks)(undirectedLinks)
    const canvas = { width: $(canvasSelector).width(), height: $(canvasSelector).height() }
    const svg = d3.select(canvasSelector).append('svg')
      .attr('id', id)
      .attr('width', canvas.width)
      .attr('height', canvas.height)

    const simulation = mkSimulation(canvas, nodes)
    drawLibrary(id, svg, linkTypes)
    const directedLink = drawLinks(id, svg, directedLinks)
    const undirectedLink = drawUndirectedLinks(id, svg, undirectedLinks)
    const node = drawNodes(svg, nodes, nodeTypes, simulation)

    simulation
      .nodes(nodes)
      .on('tick', ticked(directedLink, undirectedLink, node))

    // We're getting from a heterogenous map so we just use `any` for now
    const linkForce: any = (simulation.force('link'): any)
    linkForce.links(links)

    mkLegend(id, svg, nodeTypes, linkTypes)

    return simulation
  })
}

const dragStarted = simulation => d => {
  simulation.alphaTarget(0.3).restart()
  d.fx = d.x
  d.fy = d.y
}

const dragged = d => {
  d.fx = d3.event.x
  d.fy = d3.event.y
}

const dblClicked = function (d) {
  d3.select(this).select('polygon').classed('fixed', false)
  d.fx = null
  d.fy = null
}

const dragEnded = function (simulation) {
  return function () {
    d3.select(this).select('polygon').classed('fixed', true)
    simulation.alphaTarget(0)
  }
}

const mkLegend = <NTy: string, LTy: string>(id: string, svg: SelectWithoutData, nodeTypes: Array<NodeType<NTy>>, linkTypes: Array<LinkType<LTy>>) => {
  const legendNode = svg.append('g')
    .attr('transform', 'translate(80, 50)')
    .selectAll('.legend')
    .data(nodeTypes)
    .enter().append('g')
    .classed('legend', true)
    .attr('transform', (_, i: number) => 'translate(0, ' + i * 30 + ')')

  legendNode.append('polygon')
    .attr('points', d => drawNode(nodeTypes)(d.type))

  drawTSpans(d => d.label, RADIUS + 5)(legendNode.append('text'))

  const legendLink = svg.append('g')
    .attr('transform', 'translate(80, 180)')
    .selectAll('.legend')
    .data(S.map(forceLinkProps)(linkTypes))
    .enter().append('g')
    .classed('legend', true)
    .attr('transform', (_, i: number) => 'translate(0, ' + i * 30 + ')')

  legendLink.append('path')
    .attr('d', linkArc({ source: { x: -RADIUS, y: RADIUS }, target: { x: RADIUS, y: -RADIUS } }))
    .attr('class', d => d.type)
    .attr('stroke', d => `url(#${id}-gradient-${d.type}`)
    .classed('link', true)

  drawTSpans(d => d.label, RADIUS + 5)(legendLink.append('text'))
}

const drawTSpans = <T>(fn: T => string, x: number): (SelectWithData<T> => any) => {
  return el =>
    el.selectAll('tspan')
      .data(d => asciiStringSplit(fn(d), LABEL_LENGTH))
      .enter().append('tspan')
      .attr('y', (_, i: number) => 0.5 + 1.6 * i + 'ex')
      .attr('x', x)
      .text(d => d)
}

const close = (map: Simulation<Node<*>, Link<*>>) => () => {
  $('#arg-map a').removeAttr('style')
  $('#underlay').removeClass('inactive')
  $('#overlay').addClass('inactive')
  history.replaceState(null, '', window.location.pathname)
  map.stop()
}

const open = (map: Simulation<Node<*>, Link<*>>) => (evt: ?JQueryMouseEventObject) => {
  $('#underlay').addClass('inactive')
  $('#overlay').removeClass('inactive')
  $('#arg-map a').click(close(map))
  $('#arg-map > svg, #overlay').click(function (e) {
    if (e.target === this) { close(map)() }
  })
  if (evt != null) {
    const id = $(evt.currentTarget).attr('id')
    if (id != null) {
      $('.node > a').removeClass('entry-point')
      $('.node > a').filter((_, a) =>
        // $FlowFixMe Doesn't know about SVG anchors
        a.href.baseVal === '#' + id
      ).addClass('entry-point')
    }
  }
}

const maps: { [string]: Simulation<Node<*>, Link<*>> } = {}

const addClickHandler = () => $('a.arg-map').click(evt => {
  const id = $(evt.currentTarget).attr('href').slice(1)
  // This probably doesn't scale beautifully put I doubt it's a problem in practice ATM
  $('#overlay svg').hide()
  if (maps[id] == null) {
    mkArgMap(id, $(`a[href="#${id}"][data-data-src]`).data('data-src'), '#overlay #arg-map').then(map => {
      maps[id] = map
      open(map)(evt)
    })
  } else {
    $('#' + id).show()
    open(maps[id])(evt)
    maps[id].restart()
  }
})

const addResizeHandler = () => {
  $(window).resize(() => {
    $('#arg-map svg').width($('#arg-map').width())
    $('#arg-map svg').height($('#arg-map').height())
  })
}

const handleHash = () => {
  if (location.hash.endsWith('-map')) {
    const id = location.hash.slice(1)
    mkArgMap(id, $(`a[href="#${id}"]`).data('data-src'), '#overlay #arg-map').then(map => {
      maps[id] = map
      open(map)()
    })
  }
}

documentReadyPromise.then(() => {
  addResizeHandler()
  addClickHandler()
  handleHash()
})
