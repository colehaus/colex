"use strict";

exports.widthImpl = function(ob) {
  return function () {
    return ob.width();
  };
};
