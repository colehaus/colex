const path = require('path')
const webpack = require('webpack');
const CleanWebpackPlugin = require('clean-webpack-plugin')
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

const forProd = process.env.NODE_ENV === 'production'

const outDir = path.resolve(__dirname, process.env.OUT_DIR || 'dist')
const sourceMap = forProd ? 'source-map' : 'cheap-module-source-map'
const uglify = forProd ? [new UglifyJSPlugin({ sourceMap: true })] : []
const watch = forProd ? false : true

module.exports = {
  devtool: sourceMap,
  watch,
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
      },
      {
        test: require.resolve('jquery'),
        use: [{
          loader: 'expose-loader',
          options: 'jQuery'
        }]
      }
    ],
  },
  plugins: [
    new CleanWebpackPlugin([outDir]),
    new webpack.optimize.CommonsChunkPlugin({
      name: 'common',
      minChunks: 2
    })
  ].concat(uglify),
  entry: {
    scratch: './scratch.wp.es.js',
    quorum: './quorum.wp.es.js',
    'quorum-map': './quorum-map.wp.es.js',
    cooperatives: './cooperatives.wp.es.js',
    'graph-contents': './graph-contents.wp.es.js',
    'bibliometric-map': './bibliometric-map.wp.es.js',
    futurism: './futurism.wp.es.js',
    'custom-elements': './custom-elements.wp.es.js'
  },
  output: {
    filename: '[name].js',
    path: outDir
  }
}
