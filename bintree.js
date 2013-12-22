var __   = require('./vendor/underscore.js'),
    JSC = require('./vendor/jscheck.js');

function node(value, left, right) {
  return { value: value, left: left, right: right };
}

function insert(t, value) {
  if(!t) { return node(value); }
  if(value === t.value) { return t; }
  if(value < t.value) {
    return node(t.value, insert(t.left, value), t.right);
  }
  return node(t.value, t.left, insert(t.right, value));
}

function sorted(t) {
  var result = [];
  if(!t) { return result; }
  return sorted(t.left).concat([t.value]).concat(sorted(t.right));
}

function findWithParent(t, value, p) {
  if(!t) {
    return null;
  }

  if(t.value === value) {
    t.parent = p || null;
    return t;
  } else if(value < t.value) {
    return findWithParent(t.left, value, t);
  } else {
    return findWithParent(t.right, value, t);
  }
}

// function min(t, greaterThan) {
//   if(!t) {
//     return t;
//   }
//   if(!greaterThan) {
//     return min(t.left) || t;
//   }
//   if(t.value <= greaterThan) {
//     return min(t.right, greaterThan);
//   }
//   return min(t.left, greaterThan) || t;
// }

function min(t) {
  if(!t || !t.left) {
    return t;
  }
  return min(t.left);
}

function successor(t, value) {
  var result = findWithParent(t, value);
  if(!result) {
    return null;
  }
  if(result.right) {
    return min(result.right);
  } else {
    return min(t);
  }
}

function remove(t, value) {
  var doomed = findWithParent(t, value), replacement = null;

  if(doomed.left && doomed.right) {
    var next = min(doomed.right).value;
    remove(doomed, next);
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
        return null;
      }
    }
  }
  return t;
}

///////////////////////////////////////////////////////////////////////////

function tree(ar) {
  return __.reduce(ar, insert, null);
}

function bigTrees() {
  return JSC.array(JSC.integer(1, 100), JSC.integer(-10, 10));
}

JSC.clear();
JSC.detail(4);
JSC.on_report(console.log);

JSC.claim('Sort works and removes duplicates',
  function (verdict, vals) {
    var t = tree(vals);
    return verdict(__.isEqual(sorted(t), __.uniq(__.sortBy(vals, __.identity), true)));
  },
  bigTrees()
);

JSC.claim('Finds things present',
  function (verdict, vals) {
    var t = tree(vals),
      present = __.sample(vals);
    return verdict( !__.isNull(findWithParent(t, present)) );
  },
  bigTrees()
);

// JSC.claim('If a successor exists we can find it',
//   function (verdict, vals) {
//     var t   = tree(vals),
//       value = __.sample(vals),
//       s     = __.uniq(__.sortBy(vals, __.identity)),
//       pos   = __.indexOf(s, value, true),
//       succ  = successor(t, value);

//     var v = __.isEqual(
//         succ && succ.value,
//         ( pos < 0 ? null : (s[pos+1] || null) )
//       );

//     return verdict(v);
//   },
//   bigTrees()
// );

JSC.claim('Correctly identifies absent things',
  function (verdict, vals) {
    var t = tree(vals),
      s = sorted(t),
      absent  = __.max(s) + 1;
    return verdict( __.isNull(findWithParent(t, absent)) );
  },
  bigTrees()
);

JSC.claim('Remove gets rid of the element and no others',
  function (verdict, vals) {
    var t = tree(vals),
      s = sorted(t),
      x = __.sample(s);

    return verdict(__.isEqual(
      __.without(s, x), sorted(remove(t, x))
    ));
  },
  bigTrees()
);

JSC.check();
