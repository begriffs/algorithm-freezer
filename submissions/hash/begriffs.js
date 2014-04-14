module.exports = function () {
  return self = {

    empty: function(n) {
      return new Array(n);
    },

    hash: function(n, s) {
      return s.split('').
        reduce(
          function(total, chr) {
            return total*31 + chr.charCodeAt(0);
          }, 0
        ) % n;
    },

    insert: function(tbl, val) {
      var i = self.hash(tbl.length, val);
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
      var i = self.hash(tbl.length, val);
      if(!tbl[i]) { return false; }

      var l = tbl[i];
      do {
        if(l.val === val) { return true; }
        l = l.next;
      } while(l);

      return false;
    },

    remove: function(tbl, val) {
      var i = self.hash(tbl.length, val), l = tbl[i], prev = null;

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
