module.exports = function () {
  return self = {

    sort: function(ar) {
      if(ar.length <= 1) { return ar; }
      var pivot   = Math.floor(Math.random()*ar.length)
        , littles = ar.filter(function (val, i) { return i != pivot && val <  ar[pivot]; })
        , bigs    = ar.filter(function (val, i) { return i != pivot && val >= ar[pivot]; });
      return self.sort(littles).concat([ar[pivot]], self.sort(bigs));
    }

  };
};
