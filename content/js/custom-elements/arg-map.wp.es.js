// @flow
/* eslint no-undef: "off" */

import { drag } from 'd3-drag'
import { csv } from 'd3-fetch'
import * as sel from 'd3-selection'
import * as force from 'd3-force'
import { create, env } from 'sanctuary'

import * as shapesImport from 'libs/shapes'
import {
  asHTMLElement,
  documentReadyPromise,
  fromNullableError,
  getBySelector,
  uniquify,
  uniquifyValue
} from 'libs/util'
const S = create({ checkTypes: false, env })

type Shape =
  | 'circle'
  | 'triangle'
  | 'diamond'
  | 'square'
  | 'pentagon'
  | 'hexagon'
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

const fetchData = <NTy: string, LTy: string>(
  dataSrc: string
): Promise<
  [
    Array<Node<NTy>>,
    Array<Link<LTy>>,
    Array<NodeType<NTy>>,
    Array<LinkType<LTy>>
  ]
> =>
    Promise.all([
      csv(dataSrc + '/nodes.csv'),
      csv(dataSrc + '/links.csv'),
      csv(dataSrc + '/node-types.csv'),
      csv(dataSrc + '/link-types.csv')
    ])

const isUndirectedLink = <LTy: string>(
  links: Array<Link<LTy>>
): ((Link<LTy>) => boolean) => link =>
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
const separateDirectedAndUndirected = <LTy: string>(
  links: Array<Link<LTy>>
): [Array<Link<LTy>>, Array<Link<LTy>>] => [
    S.reject(isUndirectedLink(links))(links),
    S.pipe([
      S.filter(isUndirectedLink(links)),
      S.map(toUndirectedLink),
      uniquifyValue
    ])(links)
  ]

// Hacky way of telling Flow about d3's property additions
const forceNodeProps = <T>(t: T): T & ForceNode => (t: any)
const forceLinkProps = <T>(t: T): T & ForceLink => (t: any)

const ticked = <A: ForceLink, B: ForceNode>(
  directedLink: SelectWithData<A>,
  undirectedLink: SelectWithData<A>,
  node: SelectWithData<B>
) => () => {
    directedLink.attr('d', linkArc)
    undirectedLink.attr('d', linkArc)
    node.attr('transform', d => `translate(${d.x}, ${d.y})`)
  }

const boundingBox = (
  width: number,
  height: number,
  nodes: Array<Node<*>>
): void => {
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
  force
    .forceSimulation()
    .force(
      'link',
      force
        .forceLink()
        .id(d => d.id)
        .strength(STRENGTH)
        .distance(width / 7)
    )
    .force('charge', force.forceManyBody().strength(-250))
    .force('center', force.forceCenter(width / 2, height / 2))
    .force('bound', alpha => boundingBox(width, height, nodes))
    .alphaDecay(0.015)

const drawLibrary = <Ty: string>(
  id: string,
  svg: SelectWithoutData,
  linkTypes: Array<LinkType<Ty>>
) => {
  const uniqueLinkTypes = uniquify(linkTypes.map(d => d.type))
  // Per-type markers, as they don't inherit styles.
  const defs = svg.append('defs')
  defs
    .append('g')
    .selectAll('marker')
    .data(uniqueLinkTypes)
    .enter()
    .append('marker')
    .attr('id', d => `${id}-marker-${d}`)
    .attr('viewBox', '0 -5 10 10')
    .attr('refX', 15)
    .attr('refY', -1.5)
    .attr('markerWidth', 6)
    .attr('markerHeight', 6)
    .attr('orient', 'auto')
    .append('path')
    .attr('d', 'M0,-5L10,0L0,5')
  defs
    .append('g')
    .selectAll('linearGradient')
    .data(uniqueLinkTypes)
    .enter()
    .append('linearGradient')
    .attr('id', d => `${id}-gradient-${d}`)
    .append('stop')
}

const drawNode = <Ty: string>(
  nodeTypes: Array<NodeType<Ty>>
): (Ty => string) => {
  return type => {
    const nodeTypeIndex = nodeTypes.findIndex(t => t.type === type)
    if (nodeTypeIndex == null) {
      throw new Error(
        `Couldn't find node type ${type} in ${nodeTypes.toString()}`
      )
    }
    return shapes[nodeTypeIndex](RADIUS)
  }
}

const drawLinks = <Ty: string>(
  id: string,
  svg: SelectWithoutData,
  links: Array<Link<Ty>>
): SelectWithData<Link<Ty> & ForceLink> =>
    svg
      .append('g')
      .attr('class', 'links')
      .selectAll('path')
      .data(S.map(forceLinkProps)(links))
      .enter()
      .append('path')
      .attr('class', d => d.type)
      .classed('link', true)
      .attr('marker-end', d => `url(#${id}-marker-${d.type})`)
      .attr('stroke', d => `url(#${id}-gradient-${d.type})`)

const drawUndirectedLinks = <Ty: string>(
  id: string,
  svg: SelectWithoutData,
  links: Array<Link<Ty>>
): SelectWithData<Link<Ty> & ForceLink> =>
    svg
      .append('g')
      .attr('class', 'links')
      .selectAll('path')
      .data(S.map(forceLinkProps)(links))
      .enter()
      .append('path')
      .attr('class', d => d.type)
      .classed('link', true)
      .attr('stroke', d => `url(#${id}-gradient-${d.type})`)

const drawNodes = (
  svg: SelectWithoutData,
  nodes: Array<Node<*>>,
  nodeTypes: Array<NodeType<*>>,
  simulation: Simulation<Node<*>, Link<*>>
): SelectWithData<Node<*> & ForceNode> => {
  const node = svg
    .append('g')
    .classed('nodes', true)
    .selectAll('g.node')
    .data(S.map(forceNodeProps)(nodes))
    .enter()
    .append('g')
    .classed('node', true)
    .on('dblclick', dblClicked)
    .call(
      drag()
        .on('start', dragStarted(simulation))
        .on('drag', dragged)
        .on('end', dragEnded(simulation))
    )

  node
    .append('polygon')
    .attr('class', d => d.type)
    .attr('points', d => drawNode(nodeTypes)(d.type))

  drawTSpans(d => d.label, RADIUS + 5)(
    node
      .append('a')
      .attr('xlink:href', d => '#' + d.id)
      .append('text')
  )
  return node
}

const getWidth = S.pipe([
  getComputedStyle,
  style => style.width,
  Number.parseFloat
])

const getHeight = S.pipe([
  getComputedStyle,
  style => style.height,
  Number.parseFloat
])

const mkArgMap = (
  id: string,
  dataSrc: string,
  canvasSelector: string
): Promise<Simulation<Node<*>, Link<*>>> => {
  return fetchData(dataSrc).then(([nodes, links_, nodeTypes, linkTypes]) => {
    const canvas = getBySelector(canvasSelector)
    const [directedLinks, undirectedLinks] = separateDirectedAndUndirected(
      links_
    )
    const links = S.concat(directedLinks)(undirectedLinks)
    const canvasDims = { width: getWidth(canvas), height: getHeight(canvas) }
    const svg = sel
      .select(canvasSelector)
      .append('svg')
      .attr('id', id)
      .attr('width', canvasDims.width)
      .attr('height', canvasDims.height)

    const simulation = mkSimulation(canvasDims, nodes)
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
  d.fx = sel.event.x
  d.fy = sel.event.y
}

const dblClicked = function (d) {
  sel
    .select(this)
    .select('polygon')
    .classed('fixed', false)
  d.fx = null
  d.fy = null
}

const dragEnded = function (simulation) {
  return function () {
    sel
      .select(this)
      .select('polygon')
      .classed('fixed', true)
    simulation.alphaTarget(0)
  }
}

const mkLegend = <NTy: string, LTy: string>(
  id: string,
  svg: SelectWithoutData,
  nodeTypes: Array<NodeType<NTy>>,
  linkTypes: Array<LinkType<LTy>>
) => {
  const legendNode = svg
    .append('g')
    .attr('transform', 'translate(80, 50)')
    .selectAll('.legend')
    .data(nodeTypes)
    .enter()
    .append('g')
    .classed('legend', true)
    .attr('transform', (_, i: number) => 'translate(0, ' + i * 30 + ')')

  legendNode.append('polygon').attr('points', d => drawNode(nodeTypes)(d.type))

  drawTSpans(d => d.label, RADIUS + 5)(legendNode.append('text'))

  const legendLink = svg
    .append('g')
    .attr('transform', 'translate(80, 180)')
    .selectAll('.legend')
    .data(S.map(forceLinkProps)(linkTypes))
    .enter()
    .append('g')
    .classed('legend', true)
    .attr('transform', (_, i: number) => 'translate(0, ' + i * 30 + ')')

  legendLink
    .append('path')
    .attr(
      'd',
      linkArc({
        source: { x: -RADIUS, y: RADIUS },
        target: { x: RADIUS, y: -RADIUS }
      })
    )
    .attr('class', d => d.type)
    .attr('stroke', d => `url(#${id}-gradient-${d.type}`)
    .classed('link', true)

  drawTSpans(d => d.label, RADIUS + 5)(legendLink.append('text'))
}

const asciiStringSplit = (str: string, length: number) => {
  const result = []
  for (let i = 0; i < str.length; i += length) {
    result.push(str.substring(i, i + length))
  }
  return result
}

const drawTSpans = <T>(
  fn: T => string,
  x: number
): ((SelectWithData<T>) => any) => {
  return el =>
    el
      .selectAll('tspan')
      .data(d => asciiStringSplit(fn(d), LABEL_LENGTH))
      .enter()
      .append('tspan')
      .attr('y', (_, i: number) => 0.5 + 1.6 * i + 'ex')
      .attr('x', x)
      .text(d => d)
}

const close = (map: Simulation<Node<*>, Link<*>>) => () => {
  getBySelector('#arg-map a').removeAttribute('style')
  getBySelector('#underlay').classList.remove('inactive')
  getBySelector('#overlay').classList.add('inactive')
  history.replaceState(null, '', window.location.pathname)
  map.stop()
}

const open = (map: Simulation<Node<*>, Link<*>>) => (evt: ?MouseEvent) => {
  getBySelector('#underlay').classList.add('inactive')
  getBySelector('#overlay').classList.remove('inactive')
  document
    .querySelectorAll('#arg-map a')
    .forEach(el => el.addEventListener('click', close(map)))
  document.querySelectorAll('#arg-map > svg, #overlay').forEach(el =>
    el.addEventListener('click', function (e: Event) {
      if (e.target === this) {
        close(map)()
      }
    })
  )
  if (evt != null) {
    const id = asHTMLElement(evt.currentTarget).id
    if (id != null) {
      document
        .querySelectorAll('.node > a')
        .forEach(el => el.classList.remove('entry-point'))
      Array.from(document.querySelectorAll('.node > a'))
        .filter(
          a =>
            // $FlowFixMe Doesn't know about SVG anchors
            a.href.baseVal === '#' + id
        )
        .forEach(el => el.classList.add('entry-point'))
    }
  }
}

const maps: { [string]: Simulation<Node<*>, Link<*>> } = {}

const addClickHandler = () =>
  document.querySelectorAll('a.arg-map').forEach(e =>
    e.addEventListener('click', (evt: MouseEvent) => {
      const target = evt.currentTarget
      const id = (target instanceof HTMLAnchorElement
        ? () =>
          fromNullableError('Link without href')(
            target.getAttribute('href')
          ).slice(1)
        : () => {
          throw new Error('Somehow not a link')
        })()
      // This probably doesn't scale beautifully put I doubt it's a problem in practice ATM
      document.querySelectorAll('#overlay svg').forEach(el => {
        el.style.display = 'none'
      })
      if (maps[id] == null) {
        const dataSource = getBySelector(`a[href="#${id}"][data-data-src]`)
          .dataset.dataSrc
        mkArgMap(id, dataSource, '#overlay #arg-map').then(map => {
          maps[id] = map
          open(map)(evt)
        })
      } else {
        getBySelector('#' + id).style.display = ''
        open(maps[id])(evt)
        maps[id].restart()
      }
    })
  )

const addResizeHandler = () => {
  window.addEventListener('resize', () => {
    const svg = getBySelector('#arg-map svg')
    const argMap = getBySelector('#arg-map')
    svg.style.width = getWidth(argMap) + 'px'
    svg.style.height = getHeight(argMap) + 'px'
  })
}

const handleHash = () => {
  if (location.hash.endsWith('-map')) {
    const id = location.hash.slice(1)
    const dataSource = getBySelector(`a[href="#${id}"]`).dataset.dataSrc
    mkArgMap(id, dataSource, '#overlay #arg-map').then(map => {
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
