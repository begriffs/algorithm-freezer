var _    = require('../vendor/underscore.js')
  , jsc  = require('../vendor/jscheck.js')
  , load = require('./helper/load.js');

function arrays() {
  return jsc.array(jsc.integer(0, 10), jsc.integer(-10, 10));
}

jsc.on_report(console.log);

_.each(load.submissions('mergesort'), function (impl, author) {
  jsc.clear();
  jsc.detail(1);

  console.log("**************************************");
  console.log("Testing merge sort implementation:", author);

  jsc.claim('it sorts',
    function (verdict, vals) {
      return verdict(_.isEqual(impl.sort(vals), _.sortBy(vals, _.identity)));
    },
    arrays()
  );

  jsc.check();
});
