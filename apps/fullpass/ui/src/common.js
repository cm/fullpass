require('./fonts/lato-italic.ttf')
require('./fonts/lato-light-italic.ttf')
require('./fonts/lato-light.ttf')
require('./fonts/lato-regular.ttf')
require('./fonts/montserrat-bold.ttf')
require('./fonts/montserrat-bold.woff2')
require('./fonts/montserrat-regular.ttf')
require('./fonts/montserrat-regular.woff2')
require('./css/montserrat.css')
require('./css/lato.css')
require('./css/bootstrap.min.css')
require('./css/font-awesome.min.css')
require('./css/styles.css')
require('./js/jquery.easing.1.3.js')
require('./js/jquery.scrollTo.min.js')
require('./js/bootstrap.min.js')
require('./js/main.js')

function readCookie(cname) {
  var name = cname + "=";
  var decodedCookie = decodeURIComponent(document.cookie);
  var ca = decodedCookie.split(';');
  for(var i = 0; i <ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0) == ' ') {
      c = c.substring(1);
    }
    if (c.indexOf(name) == 0) {
      return c.substring(name.length, c.length);
    }
  }
  return "";
}


function addCookies(config) {
  config.sessionId = readCookie("cmtoken"); 
  return config;
}

module.exports = function(config) {
  config = addCookies(config);
  
  require('./elm/Main.elm')
    .Main
    .embed( document.getElementById('main'), config);
  if (module.hot) {
      module.hot.accept()
  }
}
