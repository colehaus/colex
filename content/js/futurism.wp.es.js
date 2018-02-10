// @flow

import $ from 'jquery'

import { shapes, mkMap, handler } from 'libs/arg-map'

$(() => {
  const nodeData = {
    naive: {
      label: ['Naive prediction'],
      type: 'method',
      x: w => 10,
      y: h => h / 2
    },
    mediocre: {
      label: ['Mediocre accuracy'],
      type: 'attribute',
      x: w => -w / 4,
      y: h => -20
    },
    overconfident: {
      label: ['Overconfident'],
      type: 'attribute',
      x: w => -w / 4,
      y: h => 0
    },
    optimistic: {
      label: ['Highly', 'optimistic'],
      type: 'attribute',
      x: w => -w / 4,
      y: h => 20
    },
    single: {
      label: ['Envisioning a', 'single scenario'],
      type: 'method',
      x: w => -10,
      y: h => h / 2
    },
    moreLikely: {
      label: ['Believe the', 'scenario more likely'],
      type: 'attribute',
      x: w => w / 4,
      y: h => h / 2
    },
    multiple: {
      label: ['Envisioning', 'multiple scenarios'],
      type: 'method',
      x: w => 0,
      y: h => -h / 4
    },
    outcomes: {
      label: ['Better outcomes for', 'scenario-planning', 'companies'],
      type: 'attribute',
      x: w => w / 4,
      y: h => -30
    },
    widened: {
      label: ['Widened', 'confidence intervals'],
      type: 'attribute',
      x: w => w / 4,
      y: h => -20
    },
    narrowed: {
      label: ['Narrowed', 'confidence intervals'],
      type: 'attribute',
      x: w => w / 4,
      y: h => -10
    },
    framing: {
      label: ['Reduced', 'framing bias'],
      type: 'attribute',
      x: w => w / 4,
      y: h => -40
    },
    other: {
      label: ['Other', 'planning techniques'],
      type: 'method',
      x: w => -20,
      y: h => -h / 4
    },
    pessimistic: {
      label: ['Pessimistic', 'scenarios don\'t', 'affect predictions'],
      type: 'attribute',
      x: w => w / 4,
      y: h => 20
    },
    rational: {
      label: ['Decreased rational', 'decision-making style'],
      type: 'attribute',
      x: w => w / 4,
      y: h => 10
    }
  }

  const linkData = [
    {source: 'mediocre', target: 'other', type: 'motivates'},
    {source: 'mediocre', target: 'multiple', type: 'motivates'},
    {source: 'optimistic', target: 'other', type: 'motivates'},
    {source: 'optimistic', target: 'multiple', type: 'motivates'},
    {source: 'overconfident', target: 'other', type: 'motivates'},
    {source: 'overconfident', target: 'multiple', type: 'motivates'},
    {source: 'naive', target: 'single', type: 'supertype'},
    {source: 'pessimistic', target: 'multiple', type: 'contradicts'},
    {target: 'single', source: 'moreLikely', type: 'describes'},
    {target: 'naive', source: 'overconfident', type: 'describes'},
    {target: 'naive', source: 'mediocre', type: 'describes'},
    {target: 'naive', source: 'optimistic', type: 'describes'},
    {target: 'multiple', source: 'outcomes', type: 'describes'},
    {target: 'multiple', source: 'widened', type: 'describes'},
    {target: 'multiple', source: 'narrowed', type: 'describes'},
    {target: 'multiple', source: 'framing', type: 'describes'},
    {target: 'other', source: 'framing', type: 'describes'},
    {target: 'multiple', source: 'rational', type: 'describes'}
  ]

  const nodeTypeData = [
    {type: 'attribute', label: ['Attribute'], shape: shapes.circle},
    {type: 'method', label: ['Prediction method'], shape: shapes.square}
  ]

  const linkTypeData = [
    {type: 'describes', label: ['Describes']},
    {type: 'supertype', label: ['Supertype of']},
    {type: 'contradicts', label: ['Contradicts']},
    {type: 'motivates', label: ['Motivates']}
  ]

  handler.apply(null, mkMap('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData))
})
