import $ from 'jquery'

import argMap from 'libs/arg-map'

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
  {type: 'major', label: ['Major premise'], shape: argMap.shapes.square},
  {type: 'minor', label: ['Minor premise'], shape: argMap.shapes.diamond},
  {type: 'conclusion', label: ['Conclusion'], shape: argMap.shapes.circle}
  ]

  const linkTypeData = [{type: 'imply', label: ['Jointly implies']}]

  argMap.handler(argMap.mkMap('#arg-map',
                            nodeData,
                            linkData,
                            nodeTypeData,
                            linkTypeData))
})
