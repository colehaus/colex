'use strict';

(function ($, argMap) {
  $(function () {

    var nodeData = {
      predominate: {
        label: ['CMF predominate'],
        type: 'background',
        x: function x(w) {
          return 0;
        },
        y: function y(h) {
          return h / 4;
        }
      }, efficient: {
        label: ['WC at least', 'as efficient'],
        type: 'background',
        x: function x(w) {
          return -w / 4;
        },
        y: function y(h) {
          return h / 8;
        }
      }, hypothesis: {
        label: ['WC have less', 'incentive to expand'],
        type: 'hypothesis',
        x: function x(w) {
          return 0;
        },
        y: function y(h) {
          return 0;
        }
      }, 'automaton-link': {
        label: ['Automaton'],
        type: 'model',
        x: function x(w) {
          return 0;
        },
        y: function y(h) {
          return 3 / 8 * h;
        }
      }, segment: {
        label: ['Cell represents', 'market segment'],
        type: 'attribute',
        x: function x(w) {
          return w / 8;
        },
        y: function y(h) {
          return h / 8;
        }
      }, firm: {
        label: ['Occupied by', 'CMF or WC'],
        type: 'attribute',
        x: function x(w) {
          return 0;
        },
        y: function y(h) {
          return -h / 4;
        }
      }, empty: {
        label: ['Empty segment', 'has random cost'],
        type: 'attribute',
        x: function x(w) {
          return 0;
        },
        y: function y(h) {
          return -h / 4;
        }
      }, expand: {
        label: ['Adjacent firms', 'expand into', 'empty segment'],
        type: 'attribute',
        x: function x(w) {
          return -w / 3;
        },
        y: function y(h) {
          return 0;
        }
      }, accum: {
        label: ['Profits distributed', 'each step'],
        type: 'attribute',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return -h / 8;
        }
      }, altruism: {
        label: ['WC expansion', 'depends on altruism'],
        type: 'attribute',
        x: function x(w) {
          return -w / 8;
        },
        y: function y(h) {
          return h / 4;
        }
      }
    };

    var linkData = [{ source: 'predominate', target: 'hypothesis', type: 'reconciled' }, { source: 'efficient', target: 'hypothesis', type: 'reconciled' }, { source: 'automaton-link', target: 'hypothesis', type: 'models' }, { source: 'segment', target: 'automaton-link', type: 'describes' }, { source: 'empty', target: 'segment', type: 'describes' }, { source: 'firm', target: 'segment', type: 'describes' }, { source: 'expand', target: 'empty', type: 'describes' }, { source: 'expand', target: 'firm', type: 'describes' }, { source: 'accum', target: 'firm', type: 'describes' }, { source: 'altruism', target: 'expand', type: 'describes' }, { source: 'altruism', target: 'hypothesis', type: 'describes' }];

    var nodeTypeData = [{ type: 'background', label: ['Background'], shape: argMap.shapes.square }, { type: 'hypothesis', label: ['Hypothesis'], shape: argMap.shapes.diamond }, { type: 'model', label: ['Model'], shape: argMap.shapes.circle }, { type: 'attribute', label: ['Attribute'], shape: argMap.shapes.pentagon }];

    var linkTypeData = [{ type: 'reconciled', label: ['Reconciled by'] }, { type: 'models', label: ['Models'] }, { type: 'describes', label: ['Describes'] }];

    argMap.handler(argMap.mkMap('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData));
  });
})($, argMap);
