var crypto = require('crypto')

var mkGraphHtml = (spec, tooltipOpts) => {
  spec['$schema'] = 'https://vega.github.io/schema/vega-lite/v2.json'
  spec.width = 600
  spec.height = 400
  var specString = JSON.stringify(spec)
  var randomId = 'graph-' + crypto.createHash('sha256').update(specString).digest('hex').slice(0, 7)

  var options = { renderer: 'svg' }
  return (`
    <style>.vega-actions > a { padding: 0.5em; } </style>
    <div id="${randomId}">
    </div>
    <script src="https://cdn.jsdelivr.net/npm/vega@3.0.10"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-lite@2.1.2"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-embed@3.0.0"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-tooltip@0.5.1"></script>
    <link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/npm/vega-tooltip@0.5.1/build/vega-tooltip.min.css">
    <script>
      var spec = ${specString}
      vegaEmbed('#${randomId}', spec, ${JSON.stringify(options)}).then(function(result) {
        vegaTooltip.vegaLite(result.view, spec, ${JSON.stringify(tooltipOpts)})
      })
    </script>
  `)
}

var mkRange = ([low, high], stepSize) => {
  var numSteps = ((high - low) / stepSize) + 1
  return Array.from({ length: numSteps }, (_, i) => i * stepSize + low)
}

module.exports = { mkGraphHtml, mkRange }
