var _    = require('../vendor/underscore.js')
  , jsc  = require('../vendor/jscheck.js')
  , load = require('./helper/load.js');

function fuzz() {
  return jsc.array(jsc.integer(2, 200), jsc.string());
}

jsc.on_report(console.log);

_.each(load.submissions('hash'), function (impl, author) {
  jsc.clear();
  jsc.detail(2);
  jsc.reps(1000); // some hash table problems can happen fairly rarely, so crank it up

  console.log("**************************************");
  console.log("*** Testing hash implementation:", author);

  var dict = function (ar) {
    return _.reduce(ar, impl.insert, impl.empty(100));
  };

  jsc.claim('Finds things present',
    function (verdict, vals) {
      var d = dict(vals),
        present = _.sample(vals);
      return verdict( _.isEqual(impl.find(d, present), true) );
    },
    fuzz()
  );

  jsc.claim('Correctly identifies absent things',
    function (verdict, vals) {
      var d         = dict(vals)
        , maxStrLen = _.max(vals.map(function (s) { return s.length; }))
        , absent    = new Array(maxStrLen + 1).join('x');
      return verdict( _.isEqual(impl.find(d, absent), false) );
    },
    fuzz()
  );

  jsc.claim('Removes all and only the values specified',
    function (verdict, vals) {
      var d = dict(vals)
        , bye      = _.sample(vals)
        , otherElt = _.sample(_.without(vals, bye))
        , pristine = _.clone(d);
      d = impl.remove(d, bye);
      return verdict(
        _.isEqual(impl.find(d, bye), false) &&
        _.isEqual(impl.find(d, otherElt), true)
      );
    },
    fuzz()
  );

  jsc.check();
});
