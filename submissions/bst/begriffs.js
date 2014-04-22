module.exports = function () {
  var self;
  return self = {
    node: function (value, left, right) {
      return { value: value, left: left, right: right };
    },

    insert: function (t, value) {
      if(!t) { return self.node(value); }
      if(value === t.value) { return t; }
      if(value < t.value) {
        return self.node(t.value, self.insert(t.left, value), t.right);
      }
      return self.node(t.value, t.left, self.insert(t.right, value));
    },

    sorted: function (t) {
      var result = [];
      if(!t) { return result; }
      return self.sorted(t.left).concat([t.value]).concat(self.sorted(t.right));
    },

    find: function (t, value, p) {
      if(!t) {
        return null;
      }

      if(t.value === value) {
        t.parent = p || null;
        return t;
      } else if(value < t.value) {
        return self.find(t.left, value, t);
      } else {
        return self.find(t.right, value, t);
      }
    },

    min: function (t) {
      if(!t || !t.left) {
        return t;
      }
      return self.min(t.left);
    },

    successor: function (t, value) {
      if(!t) {
        return null;
      }
      if(t.value <= value) {
        return self.successor(t.right, value);
      }
      if(t.value > value) {
        return self.successor(t.left, value) || t;
      }
    },

    remove: function (t, value) {
      var doomed = self.find(t, value), replacement = null;
      if(!doomed) { return null; }

      if(doomed.left && doomed.right) {
        var next = self.min(doomed.right).value;
        self.remove(doomed, next);
        doomed.value = next;
      } else {
        if(doomed.left) {
          replacement = doomed.left;
        } else if(doomed.right) {
          replacement = doomed.right;
        }
        var p = doomed.parent;
        if(p) {
          if(value < p.value) {
            p.left = replacement;
          } else {
            p.right = replacement;
          }
        } else {
          if(replacement) {
            doomed.value = replacement.value;
            doomed.left = replacement.left;
            doomed.right = replacement.right;
          } else {
            return t;
          }
        }
      }
      return t;
    }
  };
};
