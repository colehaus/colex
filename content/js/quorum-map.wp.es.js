import $ from 'jquery'

import argMap from 'libs/arg-map'

$(() => {
  const nodeData = {
    permissive: {
      label: ['Too permissive'],
      type: 'problem'
    },
    restrictive: {
      label: ['Too restrictive'],
      type: 'problem'
    },
    'quorum-def': {
      label: ['Traditional quorum'],
      type: 'technique'
    },
    'freq-bounds': {
      label: ['Credible bounds'],
      type: 'technique'
    },
    bayes: {
      label: ['Confidence bounds'],
      type: 'technique'
    },
    random: {
      label: ['Assumed randomness'],
      type: 'problem'
    },
    'post-hoc': {
      label: ['Only post-hoc', 'declaration', 'of quorum'],
      type: 'problem'
    },
    complicated: {
      label: ['More complicated'],
      type: 'problem'
    }
  }

  const linkData = [
  {source: 'permissive', target: 'quorum-def', type: 'describes'},
  {source: 'restrictive', target: 'quorum-def', type: 'describes'},
  {source: 'random', target: 'quorum-def', type: 'describes'},
  {source: 'random', target: 'freq-bounds', type: 'describes'},
  {source: 'random', target: 'bayes', type: 'describes'},
  {source: 'post-hoc', target: 'freq-bounds', type: 'describes'},
  {source: 'post-hoc', target: 'bayes', type: 'describes'},
  {source: 'complicated', target: 'freq-bounds', type: 'describes'},
  {source: 'complicated', target: 'bayes', type: 'describes'}
  ]

  const nodeTypeData = [
  {type: 'technique', label: ['Technique'], shape: argMap.shapes.circle},
  {type: 'problem', label: ['Problem'], shape: argMap.shapes.square}
  ]

  const linkTypeData = [
  {type: 'describes', label: ['Describes']}
  ]

  argMap.handler(argMap.mkMap('#arg-map',
                            nodeData,
                            linkData,
                            nodeTypeData,
                            linkTypeData))
})
