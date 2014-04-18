module.exports = function () {
  function swap(ar, i, j) {
    var tmp = ar[i]; ar[i] = ar[j]; ar[j] = tmp;
    return ar;
  }

  function partition(ar, l, r) {
    var nailed = l, pivot = ar[r];

    for(var i = l; i < r; i++) {
      if(ar[i] <= pivot) {
        swap(ar, i, nailed);
        nailed++;
      }
    }
    swap(ar, nailed, r);
    return nailed;
  }

  function quick(ar, l, r) {
    if(l < r) {
      var p = partition(ar, l, r);
      quick(ar, l, p-1);
      quick(ar, p+1, r);
    }
    return ar;
  }

  return {

    sort: function(ar) {
      return quick(ar, 0, ar.length-1);
    }

  };
};
