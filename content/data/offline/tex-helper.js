var nerd = require('nerdamer/all')

var wrap = content => '<pre class="example tex2jax_process"><code class="tex2jax_process"><span>\\(' + content + '\\)</span></code></pre>'

var texify = function () {
  return Array.from(arguments).map(exp => {
    if (typeof exp === 'string') {
      return wrap(nerd.convertToLaTeX(exp))
    } else {
      return wrap(exp.toTeX())
    }
  }).join('')
}

module.exports = texify
