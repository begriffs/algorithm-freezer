module.exports = function () {
  function merge(f, g) {
    var result = [];
    while(f.length > 0 && g.length > 0) {
      result.push( f[0] < g[0] ? f.shift() : g.shift() );
    }
    return result.concat(f).concat(g);
  }

  return self = {

    sort: function(ar) {
      if(ar.length <= 1) { return ar; }
      var mid = Math.floor(ar.length / 2);
      return merge(self.sort(ar.slice(0, mid)), self.sort(ar.slice(mid)));
    }

  };
};
