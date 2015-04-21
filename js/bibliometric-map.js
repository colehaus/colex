'use strict';

(function ($, argMap) {
  $(function () {

    var nodeData = {
      impact: {
        label: ['Impact factor'],
        type: 'bibliometric',
        x: function x(w) {
          return -w / 8;
        },
        y: function y(h) {
          return h / 8;
        }
      }, etc: {
        label: ['...'],
        type: 'problem',
        x: function x(w) {
          return -w / 8 - 10;
        },
        y: function y(h) {
          return h / 8 + 10;
        }
      }, review: {
        label: ['Favors review', 'articles'],
        type: 'problem',
        x: function x(w) {
          return -w / 8 - 15;
        },
        y: function y(h) {
          return h / 8 + 15;
        }
      }, 'self-cite': {
        label: ['Pressure to', 'self-cite'],
        type: 'problem',
        x: function x(w) {
          return -w / 8 - 20;
        },
        y: function y(h) {
          return h / 8 + 20;
        }
      }, replication: {
        label: ['Biased against', 'replications'],
        type: 'problem',
        x: function x(w) {
          return -w / 8 - 25;
        },
        y: function y(h) {
          return h / 8 + 25;
        }
      }, entropy: {
        label: ['Entropic'],
        type: 'bibliometric',
        x: function x(w) {
          return w / 8;
        },
        y: function y(h) {
          return -h / 8;
        }
      }, single: {
        label: ['Single study'],
        type: 'example',
        x: function x(w) {
          return w / 8 + 5;
        },
        y: function y(h) {
          return -h / 8 - 10;
        }
      }, four: {
        label: ['Four studies'],
        type: 'example',
        x: function x(w) {
          return w / 8 + 10;
        },
        y: function y(h) {
          return -h / 8 - 15;
        }
      }, aggregates: {
        label: ['Aggregates sensibly'],
        type: 'benefit',
        x: function x(w) {
          return w / 8 + 25;
        },
        y: function y(h) {
          return -h / 8 - 25;
        }
      }, design: {
        label: ['Accounts for', 'study design'],
        type: 'benefit',
        x: function x(w) {
          return w / 8 + 30;
        },
        y: function y(h) {
          return -h / 8 - 30;
        }
      }, 'repli-beni': {
        label: ['Handles replications'],
        type: 'benefit',
        x: function x(w) {
          return w / 8 + 35;
        },
        y: function y(h) {
          return -h / 8 - 35;
        }
      }, gradated: {
        label: ['Gradated citations'],
        type: 'benefit',
        x: function x(w) {
          return w / 8 + 40;
        },
        y: function y(h) {
          return -h / 8 - 40;
        }
      }, complicated: {
        label: ['Relatively complicated', 'to calculate'],
        type: 'problem',
        x: function x(w) {
          return w / 8 + 45;
        },
        y: function y(h) {
          return -h / 8 - 45;
        }
      }, dependence: {
        label: ['Relies on', 'degree of dependence'],
        type: 'problem',
        x: function x(w) {
          return w / 8 + 50;
        },
        y: function y(h) {
          return -h / 8 - 50;
        }
      }, incentive: {
        label: ['Not incentive-compatible'],
        type: 'problem',
        x: function x(w) {
          return w / 8 + 55;
        },
        y: function y(h) {
          return -h / 8 - 55;
        }
      }
    };

    var linkData = [{ source: 'review', target: 'impact', type: 'describes' }, { source: 'self-cite', target: 'impact', type: 'describes' }, { source: 'replication', target: 'impact', type: 'describes' }, { source: 'etc', target: 'impact', type: 'describes' }, { source: 'single', target: 'entropy', type: 'illustrates' }, { source: 'four', target: 'entropy', type: 'illustrates' }, { source: 'aggregates', target: 'entropy', type: 'describes' }, { source: 'design', target: 'entropy', type: 'describes' }, { source: 'repli-beni', target: 'entropy', type: 'describes' }, { source: 'gradated', target: 'entropy', type: 'describes' }, { source: 'complicated', target: 'entropy', type: 'describes' }, { source: 'dependence', target: 'entropy', type: 'describes' }, { source: 'incentive', target: 'entropy', type: 'describes' }];

    var nodeTypeData = [{ type: 'problem', label: ['Problem'], shape: argMap.shapes.square }, { type: 'benefit', label: ['Benefit'], shape: argMap.shapes.diamond }, { type: 'bibliometric', label: ['Bibliometric'], shape: argMap.shapes.circle }, { type: 'example', label: ['Example'], shape: argMap.shapes.pentagon }];

    var linkTypeData = [{ type: 'illustrates', label: ['Illustrates'] }, { type: 'describes', label: ['Describes'] }];

    argMap.handler(argMap.mkMap('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData));
  });
})($, argMap);

