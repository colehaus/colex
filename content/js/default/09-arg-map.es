const argMap = (($, d3) => {

if (!Array.prototype.find) {
  Array.prototype.find = function (predicate) {
    if (this === null) {
      throw new TypeError('Array.prototype.find called on null or undefined');
    }
    if (typeof predicate !== 'function') {
      throw new TypeError('predicate must be a function');
    }
    const list = Object(this);
    const length = list.length >>> 0;
    const thisArg = arguments[1];
    let value;

    for (let i = 0; i < length; i++) {
      value = list[i];
      if (predicate.call(thisArg, value, i, list)) {
        return value;
      }
    }
    return undefined;
  };
}

const uniq = a => {
  let seen = {};
  return a.filter(item => 
    seen.hasOwnProperty(item) ? false : (seen[item] = true)
  );
};

const shape = as =>
  r => as.map(a => Math.cos(a) * r + ' ' + Math.sin(a) * r).join(', ');
  
const triangle = shape([3 / 6 * Math.PI, 7 / 6 * Math.PI, 11 / 6 * Math.PI]);
const square = shape([
  1 / 4 * Math.PI,
  3 / 4 * Math.PI,
  5 / 4 * Math.PI,
  7 / 4 * Math.PI
]);
const diamond = shape([
  0 / 2 * Math.PI,
  1 / 2 * Math.PI,
  2 / 2 * Math.PI,
  3 / 2 * Math.PI
]);
const pentagon = shape([
  3 / 10 * Math.PI,
  7 / 10 * Math.PI,
  11 / 10 * Math.PI,
  15 / 10 * Math.PI,
  19 / 10 * Math.PI,
]);
const hexagon = shape([
  1 / 6 * Math.PI,
  3 / 6 * Math.PI,
  5 / 6 * Math.PI,
  7 / 6 * Math.PI,
  9 / 6 * Math.PI,
  11 / 6 * Math.PI
]);
const circle = r => {
  const sides = 16;
  let angles = [];
  for (let i = 0; i < sides; i++) {
    angles.push(2 * Math.PI / sides * i);
  }
  return shape(angles)(r);
};
const linkArc = ({target, source}) => {
  const dx = target.x - source.x;
  const dy = target.y - source.y;
  const dr = Math.sqrt(dx * dx + dy * dy);
  return 'M' + source.x + ',' + source.y +
    'A' + dr + ',' + dr + ' 0 0,1 ' +
    target.x + ',' + target.y;
};

//Purifying is non-trivial since building svg in memory doesn't seem to work
const mkMap = (canvasId, nodeData, linkData, nodeTypeData, linkTypeData) => {

  const canvas = $(canvasId);
  const width = canvas.width();
  const height = canvas.height();
  const defLength = width / 5;
  const xCenter = width / 2;
  const yCenter = height / 2;
  const defStrength = 0.2;
  const radius = 13;

  const mkLegend = (nodeTypeData, linkTypeData) => {

    svg.append('g')
      .attr('transform', 'translate(20, 50)')
      .classed('legend', true)
      .selectAll('.legend')
      .data(nodeTypeData)
      .enter().append('g')
        .attr('transform', (_, i) => 'translate(0, ' + i * 30 + ')')
        .each(function (d) {
          const nodeType = nodeTypeData.find(t => t.type === d.type);
          d3.select(this).append('polygon').attr('points', nodeType.shape(radius));
          d.label = d.label || [];
          const text = d3.select(this).append('text')
            .attr('x', radius + 5);
          d.label.forEach((t, i) => {
            text.append('tspan')
              .attr('y', 0.5 + 1.6 * i + 'ex')
              .classed('node', true)
              .text(t);
          });
        });

    svg.append('g')
      .attr('transform', 'translate(20, 180)')
      .classed('legend', true)
      .selectAll('.legend')
      .data(linkTypeData)
      .enter().append('g')
        .attr('transform', (_, i) => 'translate(0, ' + i * 30 + ')')
        .each(function (d) {
          d3.select(this).append('path')
            .attr('d', linkArc({source: {x: -radius, y: radius},
                                target: {x: radius, y: -radius}}))
            .classed(d.type, true)
            .classed('link', true);
          d.label = d.label || [];
          const text = d3.select(this).append('text')
            .attr('x', radius + 5);
          d.label.forEach((t, i) => {
            text.append('tspan')
              .attr('y', 0.5 + 1.6 * i + 'ex')
              .classed('node', true)
              .text(t);
          });
        });
  };

  const mkForce = (nodeData, linkData) => {

    for (let prop in nodeData) {
      if (nodeData.hasOwnProperty(prop) && nodeData[prop].url === undefined) {
        nodeData[prop].url = '#' + prop;
      }
    }
  
    const tick = () => {
      const transform = ({x, y}) => 'translate(' + x + ',' + y + ')';

      const linkCenter = ({target, source}) => {
        const xAvg = (target.x + source.x) / 2;
        const yAvg = (target.y + source.y) / 2;
        return 'translate(' + xAvg + ',' + yAvg + ')';
      };

      links.attr('d', linkArc);
      nodes.attr('transform', transform);
      nodeTexts.attr('transform', transform);
      linkTexts.attr('transform', linkCenter);
    };

    const linkTypes = uniq(linkData.map(({type}) => type));
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
        .attr('d', 'M0,-5L10,0L0,5');
   
    const force = d3.layout.force()
        .nodes(d3.values(nodeData))
        .links(linkData)
        .size([width, height])
        .linkDistance(({length}) => length || defLength)
        .linkStrength(({strength}) => strength || defStrength)
        .gravity(0.05)
        .charge(-300)
        .on('tick', tick);

    const links = svg.append('g').selectAll('path')
        .data(force.links())
      .enter().append('path')
        .attr('class', ({type}) => 'link ' + type)
        .attr('marker-end', ({type}) => 'url(#' + type + ')');

    const linkTexts = svg.append('g').selectAll('text')
        .data(force.links())
      .enter().append('text')
      .each(function (d) {
        d.label = d.label || [];
        d.label.forEach((t, i) => {
          d3.select(this).append('tspan')
            .classed('link', true)
            .text(t);
        });
      });

    const nodes = svg.append('g').selectAll('polygon')
        .data(force.nodes())
      .enter().append('polygon')
        .attr('class', ({type}) => type)
        .attr('points', d =>
          nodeTypeData.find(t => t.type === d.type).shape(radius)
        ).on('dblclick', function (d) {
          d3.select(this).classed('fixed', d.fixed = false);
        }).call(force.drag().on('dragend', function (d) {
          d3.select(this).classed('fixed', d.fixed = true);
        }));

    const nodeTexts = svg.append('g').selectAll('a')
        .data(force.nodes())
      .enter().append('a')
        .attr('xlink:href', ({url}) => url)
      .append('text')
      .each(function (d) {
        d.label = d.label || [];
        d.label.forEach((t, i) => {
          d3.select(this).append('tspan')
             .attr('x', 10)
             .attr('y', 1.6 * i + 'ex')
             .classed('node', true)
             .text(t);
        });
      });
    return force;
  };

  linkData.forEach(link => {
    if (typeof nodeData[link.source] === 'undefined' ||
        typeof nodeData[link.target] === 'undefined') {
      throw 'Linking to non-existent node';
    }
    link.source = nodeData[link.source];
    link.target = nodeData[link.target];
  });

  for (let prop in nodeData) {
    if(nodeData.hasOwnProperty(prop)) {
      if (typeof nodeData[prop].x !== 'undefined') {
        nodeData[prop].x = nodeData[prop].x(width) + xCenter;
      }
      if (typeof nodeData[prop].y !== 'undefined'){
        nodeData[prop].y = nodeData[prop].y(height) + yCenter;
      }
    }
  }

  const svg = d3.select(canvasId).append('svg')
    .attr('width', width)
    .attr('height', height);

  mkLegend(nodeTypeData, linkTypeData);
  return mkForce(nodeData, linkData);
};

const handler = map => {

  let first = true;
  const close = () => {
    $('#arg-map a').removeAttr('style');
    $('#underlay').toggleClass('inactive');
    $('#overlay').toggleClass('inactive');
    map.stop();
  };
    
  $('a[href="#arg-map"]').click(e => {
    $('#underlay').toggleClass('inactive');
    $('#overlay').toggleClass('inactive');
    if (first) {
      map.start();
      $('#arg-map a').click(close);
      $('#arg-map > svg, #overlay').click(function (e) {
        if (e.target === this) { close(); }
      });
      first = false;
    } else {
      map.resume();
    }
    // SVG requires that we not quote id here?
    $('a[href=#' + $(e.target).attr('id') + ']').css('font-weight', 'bold');
  });

};
  
return {shapes: {square, circle, diamond, triangle, hexagon, pentagon},
        mkMap, handler};

})($, d3);
