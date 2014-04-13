module.exports = function () {
  return {

    empty: function () {
      return { items: [], min: 0 };
    },

    insert: function (q, val) {
      return {
        min: val < q.items[q.min] ? q.items.length : q.min,
        items: q.items.concat([val])
      };
    },

    min: function (q) {
      return q.items.length > 0 ? q.items[q.min] : Infinity;
    },

    remove_min: function (q) {
      if(q.items.length < 1) { return q; }
      q.items = q.items.slice(0, q.min).concat(q.items.slice(q.min+1));
      q.min   = 0;
      for(var i = 0; i < q.items.length; i++) {
        if(q.items[i] < q.items[q.min]) {
          q.min = i;
        }
      }
      return q;
    }

  };
};
