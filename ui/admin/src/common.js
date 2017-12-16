require('./css/icomoon.css');
require('spectre.css/dist/spectre.min.css');
require('spectre.css/dist/spectre-exp.min.css');


module.exports = function(config) {
  require('./elm/Main.elm')
    .Main
    .embed( document.getElementById("main"), config);
  if (module.hot) {
      module.hot.accept()
  }
}
