var crypto = require('crypto')

var mkGraphHtml = (json) => {
  json['$schema'] = 'https://vega.github.io/schema/vega-lite/v2.json'
  json.width = 600
  json.height = 400
  var jsonString = JSON.stringify(json)
  var randomId = 'graph-' + crypto.createHash('sha256').update(jsonString).digest('hex').slice(0,7)

  var options = { renderer: 'svg' }
  return (`
    <style>.vega-actions > a { padding: 0.5em; } </style>
    <div id="${randomId}">
    </div>
    <script src="https://cdn.jsdelivr.net/npm/vega@3.0.10"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-lite@2.1.2"></script>
    <script src="https://cdn.jsdelivr.net/npm/vega-embed@3.0.0"></script>
    <script>vegaEmbed('#${randomId}', ${jsonString}, ${JSON.stringify(options)})</script>
  `)
}

var mkRange = ([low, high], stepSize) => {
  var numSteps = ((high - low) / stepSize) + 1
  return Array.from({length: numSteps}, (_, i) => i * stepSize + low)
}

module.exports = { mkGraphHtml, mkRange }
