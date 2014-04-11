var _  = require('../../vendor/underscore.js')
  , fs = require('fs');

module.exports.submissions = function (algo) {
  return _.reduce(
    _.filter(
      fs.readdirSync(__dirname + '/../../submissions/' + algo),
      function (file) { return file.match(/\.js$/); }
    ),
    function (obj, file) {
      var x = {};
      x[file.replace('.js', '')] = require(
        ['..', '..', 'submissions', algo, file].join('/')
      )();
      return _.extend({}, obj, x);
    },
    {}
  );
};
