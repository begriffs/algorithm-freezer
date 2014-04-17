module.exports = function () {
  function parent(i) {
    return i < 2 ? -1 : Math.floor(i/2);
  }

  function leftChild(i) { return i*2; }

  function swap(ar, i, j) {
    var tmp = ar[i]; ar[i] = ar[j]; ar[j] = tmp;
    return ar;
  }

  function bubbleUp(q, i) {
    if(i < 2) { return q; }

    if(q[i] < q[parent(i)]) {
      return bubbleUp(swap(q, i, parent(i)), parent(i));
    }
  }

  function bubbleDown(q, i) {
    var min = i
      , kid = leftChild(i);

    for(var sibling = 0; sibling <= 1; sibling++) {
      if(kid + sibling < q.length) {
        if(q[kid + sibling] < q[min]) {
          min = kid + sibling;
        }
      }
    }
    if(i !== min) {
      return bubbleDown(swap(q, i, min), min);
    }
    return q;
  }

  var self;
  return self = {

    fromArray: function (ar) {
      // clone array and shift its elements right
      var q = [null].concat(ar.slice(0));

      for(var i = q.length-1; i > 0; i--) {
        q = bubbleDown(q, i);
      }
      return q;
    },

    insert: function (q, value) {
      q.push(value);
      return bubbleUp(q, q.length - 1);
    },

    min: function (q) {
      // zeroeth element is unused for arithmetic convenience
      return q.length < 2 ? Infinity : q[1];
    },

    remove_min: function (q) {
      if(q.length < 2) { return q; }

      q[1] = q.pop();
      return bubbleDown(q, 1);
    }

  };
};
