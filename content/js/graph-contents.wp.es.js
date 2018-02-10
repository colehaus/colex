// @flow

import $ from 'jquery'

import { shapes, mkMap, handler } from 'libs/arg-map'

$(() => {
  const nodeData = {
    major: {
      label: ['All men are mortal'],
      type: 'major'
    },
    minor: {
      label: ['Socrates is a man'],
      type: 'minor'
    },
    conclusion: {
      label: ['Socrates is mortal'],
      type: 'conclusion'
    }
  }

  const linkData = [
    {source: 'major', target: 'conclusion', type: 'imply'},
    {source: 'minor', target: 'conclusion', type: 'imply'}
  ]

  const nodeTypeData = [
    {type: 'major', label: ['Major premise'], shape: shapes.square},
    {type: 'minor', label: ['Minor premise'], shape: shapes.diamond},
    {type: 'conclusion', label: ['Conclusion'], shape: shapes.circle}
  ]

  const linkTypeData = [{type: 'imply', label: ['Jointly implies']}]

  handler(mkMap('#arg-map', nodeData, linkData, nodeTypeData, linkTypeData))
})
