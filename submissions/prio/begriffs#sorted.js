module.exports = function () {
  return {

    empty: function () {
      return [];
    },

    insert: function (q, val) {
      for(var i in q) {
        if(q[i] > val) {
          return q.slice(0, i).concat([val]).concat(q.slice(i));
        }
      }
      return q.concat([val]);
    },

    min: function (q) {
      return q.length > 0 ? q[0] : Infinity;
    },

    remove_min: function (q) {
      return q.slice(1);
    }

  };
};
