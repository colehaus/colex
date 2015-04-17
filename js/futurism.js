(function($, argMap) {
"use strict";
$(function() {

var nodeData = {
  naive: {
    label: ['Naive prediction'],
    type: 'method',
    x: function(width) { return 10; },
    y: function(height) { return height / 2; }
  }, mediocre: {
    label: ['Mediocre accuracy'],
    type: 'outcome',
    x: function(width) { return -width / 4; },
    y: function(height) { return -20; }
  }, overconfident: {
    label: ['Overconfident'],
    type: 'outcome',
    x: function(width) { return -width / 4; },
    y: function(height) { return 0; }
  }, optimistic: {
    label: ['Highly', 'optimistic'],
    type: 'outcome',
    x: function(width) { return -width / 4; },
    y: function(height) { return 20; }
  }, single: {
    label: ['Envisioning a', 'single scenario'],
    type: 'method',
    x: function(width) { return -10; },
    y: function(height) { return height / 2; }
  }, moreLikely: {
    label: ['Believing the', 'scenario more likely'],
    type: 'outcome',
    x: function(width) { return width / 4; },
    y: function(height) { return height / 2; }
  }, multiple: {
    label: ['Envisioning', 'multiple scenarios'],
    type: 'method',
    x: function(width) { return 0; },
    y: function(height) { return -height / 4; }
  }, outcomes: {
    label: ['Better outcomes for', 'scenario-planning', 'companies'],
    type: 'outcome',
    x: function(width) { return width / 4; },
    y: function(height) { return -30; }
  }, widened: {
    label: ['Widened', 'confidence intervals'],
    type: 'outcome',
    x: function(width) { return width / 4; },
    y: function(height) { return -20; }
  }, narrowed: {
    label: ['Narrowed', 'confidence intervals'],
    type: 'outcome',
    x: function(width) { return width / 4; },
    y: function(height) { return -10; }
  }, framing: {
    label: ['Reduced', 'framing bias'],
    type: 'outcome',
    x: function(width) { return width / 4; },
    y: function(height) { return -40; }
  }, other: {
    label: ['Other', 'planning techniques'],
    type: 'method',
    x: function(width) { return -20; },
    y: function(height) { return -height / 4; }
  }, pessimistic: {
    label: ['Pessimistic', 'scenarios don\'t',  'affect predictions'],
    type: 'conclusion',
    x: function(width) { return width / 4; },
    y: function(height) { return 20; }
  }, rational: {
    label: ['Decreased rational', 'decision-making style'],
    type: 'outcome',
    x: function(width) { return width / 4; },
    y: function(height) { return 10; }
  }
};
for (var prop in nodeData) {
  if(nodeData.hasOwnProperty(prop)) {
    nodeData[prop].url = '#' + prop;
  }
}
var linkData = [
  {source: 'mediocre', target: 'other', type: 'motivates'},
  {source: 'mediocre', target: 'multiple', type: 'motivates'},
  {source: 'optimistic', target: 'other', type: 'motivates'},
  {source: 'optimistic', target: 'multiple', type: 'motivates'},
  {source: 'overconfident', target: 'other', type: 'motivates'},
  {source: 'overconfident', target: 'multiple', type: 'motivates'},
  {source: 'single', target: 'moreLikely', type: 'causes'},
  {source: 'naive', target: 'single', type: 'subtype'},
  {source: 'naive', target: 'overconfident', type: 'causes'},
  {source: 'naive', target: 'mediocre', type: 'causes'},
  {source: 'naive', target: 'optimistic', type: 'causes'},
  {source: 'multiple', target: 'outcomes', type: 'causes'},
  {source: 'multiple', target: 'widened', type: 'causes'},
  {source: 'multiple', target: 'narrowed', type: 'causes'},
  {source: 'multiple', target: 'framing', type: 'causes'},
  {source: 'other', target: 'framing', type: 'causes'},
  {source: 'pessimistic', target: 'multiple', type: 'contradicts'},
  {source: 'multiple', target: 'rational', type: 'causes'}
];

var nodeTypeData = [
  {type: 'outcome', label: ['Outcome'], shape: argMap.circle},
  {type: 'method', label: ['Prediction method'], shape: argMap.square},
  {type: 'conclusion', label: ['Conclusion'], shape: argMap.diamond}
];

var linkTypeData = [
  {type: 'causes', label: ['Possible causation']},
  {type: 'subtype', label: ['Subtype']},
  {type: 'contradicts', label: ['Contradict']},
  {type: 'motivates', label: ['Motivate']}
];

var first = true;
var map;

$('a[href="#arg-map"]').click(function(e) {
  $('#underlay').toggleClass('inactive');
  $('#overlay').toggleClass('inactive');
  if (first) {
    map = argMap.mkMap('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData);
    map.start();
    $('#arg-map a').click(function() {
      $('#arg-map a').removeAttr('style');
      $('#underlay').toggleClass('inactive');
      $('#overlay').toggleClass('inactive');
      map.stop();
    });
    first = false;
  } else {
    map.resume();
  }
  // SVG requires that we not quote id here?
  $('a[href=#' + $(e.target).attr('id') + ']').css('font-weight', 'bold');
});
});
})($, argMap);
