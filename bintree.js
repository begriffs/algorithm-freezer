var _   = require('./vendor/underscore.js'),
    JSC = require('./vendor/jscheck.js');

function node(value, left, right) {
  return { value: value, left: left, right: right };
}

function insert(t, value) {
  if(!t) { return node(value); }
  if(value == t.value) { return t; }
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

function tree(ar) {
  return _.reduce(ar, insert, null);
}

JSC.clear();
JSC.on_report(console.log);

JSC.claim('Sort works and removes duplicates',
  function (verdict, vals) {
    var t = tree(vals);
    return verdict(_.isEqual(sorted(t), _.uniq(_.sortBy(vals, _.identity), true)));
  },
  JSC.array(JSC.integer(1, 100), JSC.integer(-10, 10))
);

JSC.check();
