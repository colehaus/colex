var argMap = (function($, d3) {
'use strict';

var uniq = function(a) {
    var seen = {};
    return a.filter(function(item) {
        return seen.hasOwnProperty(item) ? false : (seen[item] = true);
    });
};

if (!Array.prototype.find) {
  Array.prototype.find = function(predicate) {
    if (this === null) {
      throw new TypeError('Array.prototype.find called on null or undefined');
    }
    if (typeof predicate !== 'function') {
      throw new TypeError('predicate must be a function');
    }
    var list = Object(this);
    var length = list.length >>> 0;
    var thisArg = arguments[1];
    var value;

    for (var i = 0; i < length; i++) {
      value = list[i];
      if (predicate.call(thisArg, value, i, list)) {
        return value;
      }
    }
    return undefined;
  };
}
  
var square = function(r) {
  return -r + ' ' + -r + ', ' +
         r + ' ' + -r + ', ' +
         r + ' ' + r + ', ' +
         -r + ' ' + r;
};
var circle = function(r) {
  var sides = 16;
  var circle_ = [];
  for (var i = 0; i < sides; i++) {
    circle_.push(r * Math.cos(2 * Math.PI / sides * i) + ' ' +
                 r * Math.sin(2 * Math.PI / sides * i));
  }
  return circle_.join(', ');
};
var diamond = function(r) {
  return -r + ' ' + 0 + ', ' +
         0 + ' ' + r + ', ' +
         r + ' ' + 0 + ', ' +
         0 + ' ' + -r;
};
var linkArc = function(d) {
  var dx = d.target.x - d.source.x,
      dy = d.target.y - d.source.y,
      dr = Math.sqrt(dx * dx + dy * dy);
  return 'M' + d.source.x + ',' + d.source.y + 'A' + dr + ',' + dr + ' 0 0,1 ' + d.target.x + ',' + d.target.y;
};

//Purifying is non-trivial since building svg in memory doesn't seem to work
var mkMap = function(canvasId, nodeData, linkData, nodeTypeData, linkTypeData) {

  var canvas = $(canvasId);
  var width = canvas.width();
  var height = canvas.height();
  var defLength = width / 5;
  var xCenter = width / 2;
  var yCenter = height / 2;
  var defStrength = 0.2;
  var radius = 10;

  var mkLegend = function(nodeTypeData, linkTypeData) {

    svg.append('g')
      .attr('transform', 'translate(20, 50)')
      .classed('legend', true)
      .selectAll('.legend')
      .data(nodeTypeData)
      .enter().append('g')
        .attr('transform', function (_, i) { return 'translate(0, ' + i * 30 + ')'; })
        .each(function(d) {
          var nodeType = nodeTypeData.find(function(t) { return t.type === d.type; });
          d3.select(this).append('polygon').attr('points', nodeType.shape(radius));
          d.label = d.label || [];
          var text = d3.select(this).append('text')
            .attr('x', radius + 5);
          d.label.forEach(function(t, i) {
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
        .attr('transform', function (_, i) { return 'translate(0, ' + i * 30 + ')'; })
        .each(function(d) {
          d3.select(this).append('path')
            .attr('d', linkArc({source: {x: -radius, y: radius},
                                target: {x: radius, y: -radius}}))
            .classed(d.type, true)
            .classed('link', true);
          d.label = d.label || [];
          var text = d3.select(this).append('text')
            .attr('x', radius + 5);
          d.label.forEach(function(t, i) {
            text.append('tspan')
              .attr('y', 0.5 + 1.6 * i + 'ex')
              .classed('node', true)
              .text(t);
          });
        });
  };

  var mkForce = function(nodeData, linkData) {

    var tick = function() {
      var transform = function(d) {
        return 'translate(' + d.x + ',' + d.y + ')';
      };

      var linkCenter = function(d) {
        var xAvg = (d.target.x + d.source.x) / 2;
        var yAvg = (d.target.y + d.source.y) / 2;
        return 'translate(' + xAvg + ',' + yAvg + ')';
      };

      links.attr('d', linkArc);
      nodes.attr('transform', transform);
      nodeTexts.attr('transform', transform);
      linkTexts.attr('transform', linkCenter);
    };

    var linkTypes = uniq(linkData.map(function(l) { return l.type; }));
    // Per-type markers, as they don't inherit styles.
    svg.append('defs').selectAll('marker')
        .data(linkTypes)
      .enter().append('marker')
        .attr('id', function(d) { return d; })
        .attr('viewBox', '0 -5 10 10')
        .attr('refX', 15)
        .attr('refY', -1.5)
        .attr('markerWidth', 6)
        .attr('markerHeight', 6)
        .attr('orient', 'auto')
      .append('path')
        .attr('d', 'M0,-5L10,0L0,5');

    var force = d3.layout.force()
        .nodes(d3.values(nodeData))
        .links(linkData)
        .size([width, height])
        .linkDistance(function(l) {
          return l.length || defLength;
        })
        .linkStrength(function(l) {
          return l.strength || defStrength;
        })
        .gravity(0.05)
        .charge(-300)
        .on('tick', tick);

    var links = svg.append('g').selectAll('path')
        .data(force.links())
      .enter().append('path')
        .attr('class', function(d) { return 'link ' + d.type; })
        .attr('marker-end', function(d) { return 'url(#' + d.type + ')'; });

    var linkTexts = svg.append('g').selectAll('text')
        .data(force.links())
      .enter().append('text')
      .each(function(d) {
        d.label = d.label || [];
        var that = this;
        d.label.forEach(function(t, i) {
          d3.select(that).append('tspan')
            .classed('link', true)
            .text(t);
        });
      });

    var nodes = svg.append('g').selectAll('polygon')
        .data(force.nodes())
      .enter().append('polygon')
        .attr('class', function(d) {
          return d.type;
        }).attr('points', function(d) {
          return nodeTypeData.find(function(t) { return t.type === d.type; }).shape(radius);
        }).on('dblclick', function(d) {
          d3.select(this).classed('fixed', d.fixed = false);
        }).call(force.drag().on('dragend', function(d) {
          d3.select(this).classed('fixed', d.fixed = true);
        }));

    var nodeTexts = svg.append('g').selectAll('a')
        .data(force.nodes())
      .enter().append('a')
        .attr('xlink:href', function(d) { return d.url; })
      .append('text')
      .each(function(d) {
        d.label = d.label || [];
        var that = this;
        d.label.forEach(function(t, i) {
          d3.select(that).append('tspan')
             .attr('x', 10)
             .attr('y', 1.6 * i + 'ex')
             .classed('node', true)
             .text(t);
        });
      });
    return force;
  };

  linkData.forEach(function(link) {
    if (typeof nodeData[link.source] === 'undefined' ||
        typeof nodeData[link.target] === 'undefined') {
      throw 'Linking to non-existent node';
    }
    link.source = nodeData[link.source];
    link.target = nodeData[link.target];
  });

  for (var prop in nodeData) {
    if(nodeData.hasOwnProperty(prop)) {
      if (typeof nodeData[prop].x !== 'undefined') {
        nodeData[prop].x = nodeData[prop].x(width) + xCenter;
      }
      if (typeof nodeData[prop].y !== 'undefined'){
        nodeData[prop].y = nodeData[prop].y(height) + yCenter;
      }
    }
  }

  var svg = d3.select(canvasId).append('svg')
    .attr('width', width)
    .attr('height', height);

  mkLegend(nodeTypeData, linkTypeData);
  return mkForce(nodeData, linkData);
};
return {square: square,
        circle: circle,
        diamond: diamond,
        mkMap: mkMap};
})($, d3);
