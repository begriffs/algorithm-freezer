## Your challenge

Implement a priority queue. Fill out these functions and save the
result to `submissions/prio/[your-github-name].js`. Run the test
to see if you got it right. `node test/prio.js`. Once your code
passes send a pull request and add your solution to the freezer!

### Priority queue template

```js
module.exports = function () {
  return self = {

    // create an empty queue
    empty: function () { },

    // add a value to the queue
    insert: function (q, value) { },

    // return the smallest value in the queue
    // or Infinity if the queue is empty
    min: function (q) { },

    // remove the smallest value, return
    // the diminished queue
    remove: function (t) { }

  };
};
```
