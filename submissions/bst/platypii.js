module.exports = function () {
  var self;
  return self = {

    // create a node of the tree
    node: function (value, left, right) {
      return { value: value, left: left, right: right };
    },

    // add a value to the tree
    insert: function (t, value) {
        if(t) {
            if(t.value < value) {
                t.right = self.insert(t.right, value);
            } else if(t.value > value) {
                t.left = self.insert(t.left, value);
            }
            return t;
        } else {
            return self.node(value, null, null);
        }
    },

    // the values in the tree as a sorted array
    sorted: function (t) {
        if(t) {
            return self.sorted(t.left).concat([t.value]).concat(self.sorted(t.right));
        } else {
            return [];
        }
    },

    // the node with given value, or null
    // (I ended up adding an optional third
    //  parameter for my internal use)
    find: function (t, value) {
        if(t) {
            if(t.value == value) {
                return t;
            } else if(t.value < value) {
                return self.find(t.right, value);
            } else {
                return self.find(t.left, value);
            }
        } else {
            return null;
        }
    },

    // the smallest value in the tree
    min: function (t) {
        if(t) {
            if( t.left ) {
                return self.min(t.left);
            } else {
                return t;
            }
        } else {
            return null;
        }
    },

    // the node with next greatest value
    successor: function (t, value) {
        if(t) {
            if(t.value <= value) {
                return self.successor(t.right, value);
            } else if(t.value > value) {
                var succ = self.successor(t.left, value);
                if(!succ || t.value < succ.value) {
                    return t;
                } else {
                    return succ;
                }
            }
        } else {
            return null;
        }
    },

    // remove node containing value
    //
    // return t, or null if either
    // value not found or t now empty
    remove: function (t, value) {
        var element = self.find(t, value);
        if(element) {
            if(t.value == value) {
                if(!t.left && !t.right) {
                    // t now empty
                    return null;
                } else if(!t.left) {
                    t.value = t.right.value;
                    t.left = t.right.left;
                    t.right = t.right.right;
                    return t;
                    // ^^^ HACK TO PASS TEST, WAS:
                    // return t.right;
                } else if(!t.right) {
                    t.value = t.left.value;
                    t.right = t.left.right;
                    t.left = t.left.left;
                    return t;
                    // return t.left;
                } else {
                    var succ = self.min(t.right, value);
                    t.value = succ.value;
                    t.right = self.remove(t.right, succ.value);
                    return t;
                }
            } else if(t.value < value && t.right) {
                t.right = self.remove(t.right, value);
                return t;
            } else if(t.value > value && t.left) {
                t.left = self.remove(t.left, value);
                return t;
            } else {
                console.log("SHOULD NOT HAPPEN")
                return null;
            }
        } else {
            // Not found
            return null;
        }
    }

  };
};