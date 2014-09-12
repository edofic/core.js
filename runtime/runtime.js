exports.mkthunk = function(f) {
  var memod, value;
  memod = false;
  value = null;
  return function() {
    if (!memod) {
      value = f();
      memod = true;
    }
    return value;
  };
};

var primitiveIO = function(f) {
  return function (world) {
      return [0, world, f()];
  };
};

exports.callJs = exports.mkthunk(function() {
  return function(js) {
    return function(world) {
      return [0, world, eval(js)];
    };
  };
});
