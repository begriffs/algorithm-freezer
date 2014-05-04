var _    = require('../vendor/underscore.js')
  , jsc  = require('../vendor/jscheck.js')
  , load = require('./helper/load.js');

function edges() {
  return jsc.array(
    jsc.integer(2, 3),
    jsc.object(
      ['from',             'to',               'directed'       ],
      [jsc.integer(0, 25), jsc.integer(0, 25), jsc.boolean(0)]
    )
  );
}

function subset(a, b) {
  var x = _.uniq(_.sortBy(a, _.identity)),
      y = _.uniq(_.sortBy(b, _.identity));
  return _.isEqual(_.intersection(y, x), x);
}

jsc.on_report(console.log);
jsc.on_lost(function(prob) {console.log(prob.exception); });
jsc.on_fail(function(x) { console.log(x.args); });

_.each(load.submissions('graph'), function (impl, author) {

  // breadth-first initial segments
  var bfis = function(g, node, visited, uncles, results) {
        // everything seen so far
    var all = visited.concat(uncles).concat([node]),
        // unseen outbound links
        out = _.difference(impl.outbound(g, node), all);

    results[node] = (results[node] || []).concat([all]);

    for(var o in out) {
      bfis( g, out[o], all, _.without(out, out[o]), results );
    }
    return results;
  }

  jsc.clear();
  jsc.detail(1);

  console.log("**************************************");
  console.log("Testing graph implementation:", author);

  jsc.claim('Constructs edges',
    function (verdict, edges) {
      var g = impl.fromEdges(edges),
        e = _.sample(edges);
      return verdict( !_.isNull(impl.connected(g, e.from, e.to)) );
    },
    edges()
  );

  jsc.claim('Knows all outbound links',
    function (verdict, edges) {
      var g = impl.fromEdges(edges),
        e = _.sample(edges);
      return verdict( !_.isNull(impl.connected(g, e.from, e.to)) );
    },
    edges()
  );

  jsc.claim('Removes edges',
    function (verdict, edges) {
      var g = impl.fromEdges(edges),
        e = _.sample(edges);
      impl.removeEdge(g, e.from, e.to, e.directed);
      return verdict( _.isNull(impl.connected(g, e.from, e.to)) );
    },
    edges()
  );

  jsc.claim('BFS visits a correct order',
    function (verdict, edges) {
      var g = impl.fromEdges(edges),
        validSegments = bfis(g, edges[0].from, [], [], {}),
        seen = [];

      impl.bfs(g, edges[0].from, {
        vertex: function(v) {
          if(!_.contains(seen, v) &&
             !_.find(validSegments[v], _.partial(subset, seen)))
          {
            console.log('expected', seen, 'as a subset of one of', validSegments[v]);
            return verdict( false );
          } else {
            seen.push(v);
          }
        }
      });
      return verdict( true );
    },
    edges()
  );

  jsc.check();
});
