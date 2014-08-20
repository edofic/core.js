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
