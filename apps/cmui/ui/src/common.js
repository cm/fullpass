require("./fonts/lato-italic.ttf")
require("./fonts/lato-light-italic.ttf")
require("./fonts/lato-light.ttf")
require("./fonts/lato-regular.ttf")
require("./fonts/montserrat-bold.ttf")
require("./fonts/montserrat-bold.woff2")
require("./fonts/montserrat-regular.ttf")
require("./fonts/montserrat-regular.woff2")
require("./css/montserrat.css");
require("./css/lato.css");
require("font-awesome/css/font-awesome.css");
//require("bootstrap/scss/bootstrap.scss");
//require("./css/now-ui-kit.css");
//require("./css/demo.css");
//window.jQuery = window.$ =  require("./js/jquery.min");
//window.Tether = require("tether");
//require("bootstrap");
//require("./js/bootstrap-switch");
//require("./js/nouislider.min");
//require("./js/bootstrap-datepicker");
//require("./js/now-ui-kit");

require("hammerjs");
require("./materialize/jquery-3.2.1.min.js");
require("./materialize/materialize.min.css");
require("./materialize/materialize.min.js");
require("./materialize/materialicons.css");
require("./materialize/hacks.css");

module.exports = function(config) {
  require('./elm/Main.elm')
    .Main
    .embed( document.getElementById("main"), config);
  if (module.hot) {
      module.hot.accept()
  }
}
