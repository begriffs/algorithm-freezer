var _    = require('../vendor/underscore.js')
  , jsc  = require('../vendor/jscheck.js')
  , load = require('./helper/load.js');

function strings() {
  return jsc.array(3, jsc.string());
}

function noSubstrs() {
  return jsc.array([
    jsc.string(jsc.integer(1, 10),  jsc.character('A', 'Z')),
    jsc.string(jsc.integer(10, 20), jsc.character('a', 'z'))
  ]);
}

jsc.on_report(console.log);
jsc.on_lost(function(prob) {
  console.log(prob.exception);
});

_.each(load.submissions('rabin-karp'), function (impl, author) {
  jsc.clear();
  jsc.detail(1);

  console.log("**************************************");
  console.log("*** Testing Rabin-Karp implementation:", author);

  jsc.claim('Finds substrings',
    function (verdict, vals) {
      var str = vals.join(''), substr = vals[1];

      return verdict( !_.isEqual(impl.find(substr, str), -1) );
    },
    strings()
  );

  jsc.claim('Rejects non-substrings',
    function (verdict, vals) {
      var littles = vals[0], bigs = vals[1];
      return verdict( _.isEqual(impl.find(littles, bigs), -1) );
    },
    noSubstrs()
  );

  jsc.check();
});
