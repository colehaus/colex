$(function() {

var draw = function() {

  $('#underlay').toggleClass('inactive');
  $('#overlay').toggleClass('inactive');
  
  var canvas = "#arg-map";
  var width = $(canvas).width();
  var height = $(canvas).height();
  var defLength = width / 5;
  var xCenter = width / 2;
  var yCenter = height / 2;
  var defStrength = 0.2;

  var linkArc = function(d) {
    var dx = d.target.x - d.source.x,
        dy = d.target.y - d.source.y,
        dr = Math.sqrt(dx * dx + dy * dy);
    return "M" + d.source.x + "," + d.source.y + "A" + dr + "," + dr + " 0 0,1 " + d.target.x + "," + d.target.y;
  };

  var transform = function(d) {
    return "translate(" + d.x + "," + d.y + ")";
  };

  var tick = function() {
    path.attr("d", linkArc);
    circle.attr("transform", transform);
    nodeText.attr("transform", transform);
    linkText.attr("transform", linkCenter);
  };
  var linkCenter = function(d) {
    var xAvg = (d.target.x + d.source.x) / 2;
    var yAvg = (d.target.y + d.source.y) / 2;
    return "translate(" + xAvg + "," + yAvg + ")";
  };

  var nodes = { Nokia: { name: "Nokia", x: xCenter + 100, y: yCenter },
                Kodak: { name: "Kodak", url: "#target1", x: xCenter, y: yCenter },
                RIM: { name : "RIM", x: xCenter, y: yCenter + 60 },
                Samsung: { name: "Samsung", x: xCenter, y: yCenter - 60},
                Boo: { name: "Boo" }
              };

  var links1 = [
    {source: "Nokia", target: "Samsung", type: "resolved"},
    {source: "RIM", target: "Kodak", type: "suit"},
    {source: "Kodak", target: "Samsung", type: "resolved"},
    {source: "Nokia", target: "RIM", type: "licensing"},
    {source: "Samsung", target: "Boo", type: "suit"}
  ];
  var links2 = [
    {source: "Nokia", target: "Samsung", type: "resolved", length: 10},
    {source: "RIM", target: "Kodak", type: "suit"},
    {source: "Kodak", target: "Samsung", type: "resolved"},
    {source: "Nokia", target: "RIM", type: "licensing", length: 10},
    {source: "Samsung", target: "Boo", type: "suit"}
  ];


  links1.forEach(function(link) {
    link.source = nodes[link.source];
    link.target = nodes[link.target];
  });
  links2.forEach(function(link) {
    link.source = nodes[link.source];
    link.target = nodes[link.target];
  });


  var force = d3.layout.force()
      .nodes(d3.values(nodes))
      .links(links1)
      .size([width, height])
      .linkDistance(function(l) {
        return l.length || defLength;
      })
      .linkStrength(function(l) {
        return l.strength || defStrength;
      })
      .charge(-300)
      .on("tick", tick)
      .start();

  var svg = d3.select(canvas).append("svg")
      .attr("width", width)
      .attr("height", height);

  // Per-type markers, as they don't inherit styles.
  svg.append("defs").selectAll("marker")
      .data(["suit", "licensing", "resolved"])
    .enter().append("marker")
      .attr("id", function(d) { return d; })
      .attr("viewBox", "0 -5 10 10")
      .attr("refX", 15)
      .attr("refY", -1.5)
      .attr("markerWidth", 6)
      .attr("markerHeight", 6)
      .attr("orient", "auto")
    .append("path")
      .attr("d", "M0,-5L10,0L0,5");

  var path = svg.append("g").selectAll("path")
      .data(force.links())
    .enter().append("path")
      .attr("class", function(d) { return "link " + d.type; })
      .attr("marker-end", function(d) { return "url(#" + d.type + ")"; });

  var circle = svg.append("g").selectAll("circle")
      .data(force.nodes())
    .enter().append("circle")
      .attr("r", 10)
      .on("dblclick", function(d) {
        d3.select(this).classed("fixed", d.fixed = false);
      })
      .call(force.drag().on("dragend", function(d) {
        d3.select(this).classed("fixed", d.fixed = true);
      }));

  var linkText = svg.append("g").selectAll("text")
      .data(force.links())
    .enter().append("text")
      .attr("class", "link")
      .text(function(d) { return d.type; });
  
  var nodeText = svg.append("g").selectAll("a")
      .data(force.nodes())
    .enter().append("a")
      .attr("xlink:href", function(d) { return d.url; })
    .append("text")
      .attr("x", 10)
      .attr("y", ".31em")
      .attr("class", "node")
      .text(function(d) { return d.name; });

  $('#arg-map a').off().click(function() {
    $('#underlay').toggleClass('inactive');
    $('#overlay').toggleClass('inactive');
  });
  
};

$('a[href="#arg-map"]').click(function() {
  draw();
});
  
});

  // var button = d3.select("button").on("click", function() {
  //   switch (toggleState) {
  //   case 1:
  //     force.links(links2).start();
  //     toggleState = 2;
  //     break;
  //   case 2:
  //     force.links(links1).start();
  //     toggleState = 1;
  //     break;
  //   }
  // });
