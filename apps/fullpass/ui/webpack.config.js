const webpack = require('webpack')
const path = require('path')
const merge  = require( 'webpack-merge' );
const copy = require('copy-webpack-plugin')

const entry = process.env.npm_lifecycle_event === 'build' ? 'prod' : 'dev';

const jqueryAliases = {
  'jquery': 'jquery',
  'jQuery': 'jquery',
  '$': 'jquery'
}


const config = {
  resolve: { alias: jqueryAliases },
  context: path.resolve(__dirname, 'src'),
  entry: "./" + entry + ".js",
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'app.js',
    publicPath: '/dist/',
  },
  plugins: [
    new webpack.NamedModulesPlugin(),
    new webpack.ProvidePlugin(jqueryAliases)
  ],
  module: {
    rules: [
      {
        test: /\.js$/,
        include: path.resolve(__dirname, 'src'),
        use: [{
          loader: 'babel-loader',
          options: {
            presets: [
              ['es2015', { modules: false }]
            ]
          }
        }]
      },
      {
        test: /\.scss$/,
        use: [
          'style-loader',
          'css-loader',
          'sass-loader'
        ]
      },
      {
        test: /\.css$/,
        use: [
          'style-loader',
          'css-loader',
        ]
      },
      {
        test   : /\.(ttf|eot|svg|woff(2)?)(\?[a-z0-9=&.]+)?$/,
        use: [
          'url-loader'

        ]
      },

      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader?verbose=true&warn=true',
      }
    ],

    noParse: /\.elm$/
  }
}

module.exports = config
