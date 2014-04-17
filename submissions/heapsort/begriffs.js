module.exports = function () {
  var H = require('../prio/begriffs#heap.js')();

  return {

    sort: function(ar) {
      var q = H.fromArray(ar), sorted = [];
      while(q.length > 1) {
        sorted.push(H.min(q));
        q = H.remove_min(q);
      }
      return sorted;
    }

  };
};
