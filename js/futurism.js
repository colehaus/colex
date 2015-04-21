'use strict';

(function ($, argMap) {
  $(function () {

    var nodeData = {
      naive: {
        label: ['Naive prediction'],
        type: 'method',
        x: function x(w) {
          return 10;
        },
        y: function y(h) {
          return h / 2;
        }
      }, mediocre: {
        label: ['Mediocre accuracy'],
        type: 'outcome',
        x: function x(w) {
          return -w / 4;
        },
        y: function y(h) {
          return -20;
        } }, overconfident: {
        label: ['Overconfident'],
        type: 'outcome',
        x: function x(w) {
          return -w / 4;
        },
        y: function y(h) {
          return 0;
        }
      }, optimistic: {
        label: ['Highly', 'optimistic'],
        type: 'outcome',
        x: function x(w) {
          return -w / 4;
        },
        y: function y(h) {
          return 20;
        }
      }, single: {
        label: ['Envisioning a', 'single scenario'],
        type: 'method',
        x: function x(w) {
          return -10;
        },
        y: function y(h) {
          return h / 2;
        }
      }, moreLikely: {
        label: ['Believing the', 'scenario more likely'],
        type: 'outcome',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return h / 2;
        }
      }, multiple: {
        label: ['Envisioning', 'multiple scenarios'],
        type: 'method',
        x: function x(w) {
          return 0;
        },
        y: function y(h) {
          return -h / 4;
        }
      }, outcomes: {
        label: ['Better outcomes for', 'scenario-planning', 'companies'],
        type: 'outcome',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return -30;
        }
      }, widened: {
        label: ['Widened', 'confidence intervals'],
        type: 'outcome',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return -20;
        }
      }, narrowed: {
        label: ['Narrowed', 'confidence intervals'],
        type: 'outcome',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return -10;
        }
      }, framing: {
        label: ['Reduced', 'framing bias'],
        type: 'outcome',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return -40;
        }
      }, other: {
        label: ['Other', 'planning techniques'],
        type: 'method',
        x: function x(w) {
          return -20;
        },
        y: function y(h) {
          return -h / 4;
        }
      }, pessimistic: {
        label: ['Pessimistic', 'scenarios don\'t', 'affect predictions'],
        type: 'conclusion',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return 20;
        }
      }, rational: {
        label: ['Decreased rational', 'decision-making style'],
        type: 'outcome',
        x: function x(w) {
          return w / 4;
        },
        y: function y(h) {
          return 10;
        }
      }
    };

    var linkData = [{ source: 'mediocre', target: 'other', type: 'motivates' }, { source: 'mediocre', target: 'multiple', type: 'motivates' }, { source: 'optimistic', target: 'other', type: 'motivates' }, { source: 'optimistic', target: 'multiple', type: 'motivates' }, { source: 'overconfident', target: 'other', type: 'motivates' }, { source: 'overconfident', target: 'multiple', type: 'motivates' }, { source: 'single', target: 'moreLikely', type: 'causes' }, { source: 'naive', target: 'single', type: 'subtype' }, { source: 'naive', target: 'overconfident', type: 'causes' }, { source: 'naive', target: 'mediocre', type: 'causes' }, { source: 'naive', target: 'optimistic', type: 'causes' }, { source: 'multiple', target: 'outcomes', type: 'causes' }, { source: 'multiple', target: 'widened', type: 'causes' }, { source: 'multiple', target: 'narrowed', type: 'causes' }, { source: 'multiple', target: 'framing', type: 'causes' }, { source: 'other', target: 'framing', type: 'causes' }, { source: 'pessimistic', target: 'multiple', type: 'contradicts' }, { source: 'multiple', target: 'rational', type: 'causes' }];

    var nodeTypeData = [{ type: 'outcome', label: ['Outcome'], shape: argMap.shapes.circle }, { type: 'method', label: ['Prediction method'], shape: argMap.shapes.square }, { type: 'conclusion', label: ['Conclusion'], shape: argMap.shapes.diamond }];

    var linkTypeData = [{ type: 'causes', label: ['Possible causation'] }, { type: 'subtype', label: ['Subtype'] }, { type: 'contradicts', label: ['Contradict'] }, { type: 'motivates', label: ['Motivate'] }];

    argMap.handler(argMap.mkMap('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData));
  });
})($, argMap);

