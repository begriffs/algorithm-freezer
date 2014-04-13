var _    = require('../vendor/underscore.js')
  , jsc  = require('../vendor/jscheck.js')
  , load = require('./helper/load.js');

function bigTrees() {
  return jsc.array(jsc.integer(3, 20), jsc.integer(-10, 10));
}

jsc.on_report(console.log);

_.each(load.submissions('bst'), function (impl, author) {
  jsc.clear();
  jsc.detail(1);

  console.log("**************************************");
  console.log("Testing BST implementation:", author);

  var tree = function (ar) {
    return _.reduce(ar, impl.insert, null);
  };

  jsc.claim('Sort works and removes duplicates',
    function (verdict, vals) {
      var t = tree(vals);
      return verdict(_.isEqual(impl.sorted(t), _.uniq(_.sortBy(vals, _.identity), true)));
    },
    bigTrees()
  );

  jsc.claim('Finds things present',
    function (verdict, vals) {
      var t = tree(vals),
        present = _.sample(vals);
      return verdict( !_.isNull(impl.find(t, present)) );
    },
    bigTrees()
  );

  jsc.claim('If a successor exists we can find it',
    function (verdict, vals) {
      var t   = tree(vals),
        value = _.sample(vals),
        s     = _.uniq(_.sortBy(vals, _.identity)),
        pos   = _.indexOf(s, value, true),
        claim = impl.successor(t, value),
        truth = null;

      if(pos >= 0 && pos+1 < s.length) {
        truth = s[pos+1];
      }

      return verdict(_.isEqual(
          claim && claim.value,
          truth
        )
      );
    },
    bigTrees()
  );

  jsc.claim('Correctly identifies absent things',
    function (verdict, vals) {
      var t = tree(vals),
        s = impl.sorted(t),
        absent  = _.max(s) + 1;
      return verdict( _.isNull(impl.find(t, absent)) );
    },
    bigTrees()
  );

  jsc.claim('Remove gets rid of the element and no others',
    function (verdict, vals) {
      var t = tree(vals),
        s = impl.sorted(t),
        x = _.sample(s);

      return verdict(_.isEqual(
        _.without(s, x), impl.sorted(impl.remove(t, x))
      ));
    },
    bigTrees()
  );

  jsc.check();
});
