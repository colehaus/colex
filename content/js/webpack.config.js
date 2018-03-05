const path = require('path')
const webpack = require('webpack')
const FlowWebpackPlugin = require('flow-webpack-plugin')
const UglifyJSPlugin = require('uglifyjs-webpack-plugin')

const forProd = process.env.NODE_ENV === 'production'

const outDir = path.resolve(__dirname, process.env.OUT_DIR || 'dist')
const sourceMap = forProd ? 'source-map' : 'cheap-module-source-map'
const uglify = forProd ? [new UglifyJSPlugin({ sourceMap: true })] : []
const watch = !forProd

module.exports = {
  devtool: sourceMap,
  watch,
  resolve: {
    extensions: ['.wp.es.js', '.es', '.js'],
    modules: process.env.NODE_PATH.split(':').concat(['js/'])
  },
  resolveLoader: {
    modules: process.env.NODE_PATH.split(':')
  },
  module: {
    rules: [
      { test: /\.js$|\.es$|\.ws$/, exclude: /node_modules/, loader: 'babel-loader' },
      { test: [
        require.resolve('jquery-flot')
      ],
        use: 'imports-loader?$=jquery,jQuery=jquery'
      },
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
    new FlowWebpackPlugin(),
    new webpack.optimize.CommonsChunkPlugin({
      name: 'common',
      minChunks: 2
    })
  ].concat(uglify),
  entry: {
    draw: './custom-elements/draw.wp.es.js',
    'util-egal': './util-egal.wp.es.js',
    quorum: './quorum.wp.es.js',
    'custom-elements': './custom-elements.wp.es.js'
  },
  output: {
    filename: '[name].js',
    path: outDir
  }
}
