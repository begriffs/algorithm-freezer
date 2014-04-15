var H = require('../hash/begriffs.js')();

module.exports = function () {

  var hashBuckets = 1000;

  return {

    find: function(needle, haystack) {
      var i
        , m            = needle.length
        , n            = haystack.length
        , signature    = H.hash(hashBuckets, needle)
        , searchWindow = haystack.slice(0, m)
        , searchHash   = H.hash(hashBuckets, searchWindow);

      for(i = 0; i < n-m; i++) {
        searchWindow = haystack.slice(i, i+m);
        if(signature === searchHash) {
          if(needle === searchWindow) { return i; }
        }
        searchHash = H.roll(hashBuckets, searchHash, searchWindow, haystack[i+m]);
      }

      return -1;
    }

  };
};
