const path = require('path')
const webpack = require('webpack');
const CleanWebpackPlugin = require('clean-webpack-plugin')
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

const outDir = path.resolve(__dirname, process.env.outDir || 'dist')

module.exports = {
  watch: true,
  resolve: {
    extensions: [".wp.es.js", ".es", ".js"],
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
      }
    ],
  },
  plugins: [
    new UglifyJSPlugin(),
    new CleanWebpackPlugin([outDir]),
    new webpack.optimize.CommonsChunkPlugin({
      name: 'common',
      minChunks: 2
    })
  ],
  entry: {
    scratch: './js/scratch.wp.es.js',
    quorum: './js/quorum.wp.es.js',
    'quorum-map': './js/quorum-map.wp.es.js',
    cooperatives: './js/cooperatives.wp.es.js',
    'graph-contents': './js/graph-contents.wp.es.js',
    'bibliometric-map': './js/bibliometric-map.wp.es.js',
    futurism: './js/futurism.wp.es.js',
    'custom-elements': './js/custom-elements.wp.es.js'
  },
  output: {
    filename: '[name].js',
    path: outDir
  }
}