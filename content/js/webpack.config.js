const path = require('path')
const which = require('which')
const FlowWebpackPlugin = require('flow-webpack-plugin')

const mode = process.env.NODE_ENV === 'production' ? 'production' : 'development'
const noWatch = process.env.NO_WATCH === 'no_watch'

const outDir = path.resolve(__dirname, process.env.OUT_DIR || 'dist')
// We rely on globally installed `flow` because NixOS doesn't like the way `npm` installs the binary
const flowPath = which.sync('flow')

module.exports = {
  optimization: { splitChunks: { chunks: 'all' } },
  mode,
  watch: mode === 'development' && !noWatch,
  resolve: {
    extensions: ['.wp.es.js', '.es', '.js'],
    modules: process.env.NODE_PATH.split(':').concat(['js/', './'])
  },
  resolveLoader: {
    modules: process.env.NODE_PATH.split(':')
  },
  module: {
    rules: [
      {
        enforce: 'pre',
        test: /\.wp.es.js$/,
        loader: 'standard-loader',
      },
      { test: /\.js$|\.es$|\.ws$/, exclude: /node_modules/, loader: 'babel-loader' },
      { test: [
        require.resolve('jquery-flot')
      ],
      use: 'imports-loader?$=jquery,jQuery=jquery'
      },
      // For purescript
      {
        test: require.resolve('jquery'),
        use: [{
          loader: 'expose-loader',
          options: 'jQuery'
        }]
      }
    ]
  },
  plugins: [
    new FlowWebpackPlugin({ flowPath }),
  ],
  entry: {
    'util-egal': './util-egal.wp.es.js',
    quorum: './quorum.wp.es.js',
    'ideal-calibration': './ideal-calibration.wp.es.js',
    'is-development-easy': './is-development-easy.wp.es.js',
    'custom-elements': './custom-elements.wp.es.js',
    'false-dichotomy-ideal-theory-debate': './false-dichotomy-ideal-theory-debate.wp.es.js'
  },
  output: {
    path: outDir
  }
}
