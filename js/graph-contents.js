(function($, argMap) {
"use strict";
$(function() {

var nodeData = {
  major: {
    label: ['All men are mortal'],
    type: 'major'
  }, minor: {
    label: ['Socrates is a man'],
    type: 'minor'
  }, conclusion: {
    label: ['Socrates is mortal'],
    type: 'conclusion'
  }
};
for (var prop in nodeData) {
  if(nodeData.hasOwnProperty(prop)) {
    nodeData[prop].url = '#' + prop;
  }
}
var linkData = [
  {source: 'major', target: 'conclusion', type: 'imply'},
  {source: 'minor', target: 'conclusion', type: 'imply'}
];

var nodeTypeData = [
  {type: 'major', label: ['Major premise'], shape: argMap.square},
  {type: 'minor', label: ['Minor premise'], shape: argMap.diamond},
  {type: 'conclusion', label: ['Conclusion'], shape: argMap.circle}
];

var linkTypeData = [{type: 'imply', label: ['Jointly implies']}];

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
