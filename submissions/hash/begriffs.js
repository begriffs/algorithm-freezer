module.exports = function () {
  function hash(s, n) {
    return s.split('').
      reduce(
        function(total, chr) {
          return total*31 + chr.charCodeAt(0);
        }, 0
      ) % n;
  }
  var buckets = 100;

  return {

    insert: function(tbl, val) {
      tbl = tbl || [];
      var i = hash(val, buckets);
      if(!tbl[i]) {
        tbl[i] = { val: val };
      } else {
        var l = tbl[i];
        while(true) {
          if(l.val === val) { return tbl; }

          if(l.next) { l = l.next; }
          else       { break;      }
        }
        l.next = { val: val };
      }
      return tbl;
    },

    find: function(tbl, val) {
      var i = hash(val, buckets);
      if(!tbl[i]) { return false; }

      var l = tbl[i];
      do {
        if(l.val === val) { return true; }
        l = l.next;
      } while(l);

      return false;
    },

    remove: function(tbl, val) {
      var i = hash(val, buckets), l = tbl[i], prev = null;

      while(l) {
        if(l.val === val) {
          if(prev) { prev.next = l.next; }
          else     { tbl[i]    = l.next; }
          return tbl;
        }
        prev = l;
        l = l.next;
      }
      return tbl;
    }

  };
};
