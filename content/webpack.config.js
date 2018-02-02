const path = require('path')
const webpack = require('webpack');
const CleanWebpackPlugin = require('clean-webpack-plugin')
const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

module.exports = env => {
  const outDir = path.resolve(__dirname, env && env.outDir || 'dist')
  return {
    module: {
      rules: [
        { test: /\.js$|\.es$|\.ws$/, exclude: /node_modules/, loader: 'babel-loader' }
      ],
    },
    plugins: [
      new UglifyJSPlugin(),
      new CleanWebpackPlugin([outDir]),
      new webpack.optimize.CommonsChunkPlugin({
        name: 'common'
      })
    ],
    entry: {
      scratch: './js/scratch.ws'
    },
    output: {
      filename: '[name].bundle.js',
      path: outDir
    }
  }
}
