module.exports = function () {

  var self;
  return self = {

    fromArray: function (ar) {
      return ar.reduce(self.insert, { items: [], minElt: 0 });
    },

    isEmpty: function (q) {
      return q.items.length === 0;
    },

    insert: function (q, val) {
      return {
        minElt: val < q.items[q.minElt] ? q.items.length : q.minElt,
        items: q.items.concat([val])
      };
    },

    min: function (q) {
      return self.isEmpty(q) ? Infinity : q.items[q.minElt];
    },

    remove_min: function (q) {
      if(q.items.length < 2) {
        q.items.pop();
        return q;
      }

      q.items  = q.items.slice(0, q.minElt).concat(q.items.slice(q.minElt+1));
      q.minElt = 0;
      for(var i = 0; i < q.items.length; i++) {
        if(q.items[i] < q.items[q.minElt]) {
          q.minElt = i;
        }
      }
      return q;
    }

  };
};
