module.exports = function () {
  var bst = require('../bst/begriffs.js')();

  return self = {

    insertEdge: function(G, x, y, directed) {
      G.edges[x] = bst.insert(G.edges[x], y);
      G.degree[x] = (G.degree[x] || 0) + 1;

      if(directed === false) {
        self.insertEdge(G, y, x, true);
      } else {
        G.nEdges++;
      }
      return G;
    },

    fromEdges: function(edges) {
      var G = { edges: {}, degree: {}, nEdges: 0 };
      for(var i in edges) {
        self.insertEdge(G, edges[i].from, edges[i].to, edges[i].directed);
      }
      return G;
    },

    removeEdge: function(G, x, y, directed) {
      if(G.edges[x] = bst.remove(G.edges[x], y)) {
        G.degree[x]--;
        if(directed === false) {
          self.removeEdge(G, y, x, true);
        } else {
          G.nEdges--;
        }
      }
      return G;
    },

    connected: function(G, x, y) {
      return bst.find(G.edges[x], y);
    },

    outbound: function(G, x) {
      return bst.sorted(G.edges[x]);
    },

    bfs: function(G, x, processors) {
      processors            = processors            || {};
      processors.edge       = processors.edge       || function() {};
      processors.vertex     = processors.vertex     || function() {};
      processors.vertexLate = processors.vertexLate || function() {};

      var processed = {}, discovered = {},
          parents = {}, toVisit = [x];
      while(toVisit.length > 0) {
        var v = toVisit.pop();
        processors.vertex(v);

        var out = self.outbound(G, v);
        for(var o in out) {
          if(!processed[out[o]] || G.directed) {
            processors.edge(v, out[o]);
          }
          if(!discovered[out[o]]) {
            toVisit.unshift(out[o]);
            discovered[out[o]] = true;
            parents[out[o]] = v;
          }
          processors.vertexLate(v);
        }
      }
      return parents;
    }

  };
};
