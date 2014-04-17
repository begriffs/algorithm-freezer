## Your challenge

Implement a min priority queue three ways:

* a heap
* an ordered list
* an unordered list

Certainly do the heap implementation because you'll use it in later
algorithms.

Fill out these functions and save the result to
`submissions/prio/[your-github-name]#type.js`. Where `type` is
ordered, unordered, or heap. Run the test to see if you got it
right. `node test/prio.js`. Once your code passes send a pull request
and add your solution to the freezer!

### Priority queue template

```js
module.exports = function () {
  var self;
  return self = {

    // queue up the array elements
    fromArray: function (ar) { },

    // boolean
    isEmpty: function (q) { },

    // add a value to the queue
    insert: function (q, value) { },

    // return the smallest value in the queue
    // or Infinity if the queue is empty
    min: function (q) { },

    // remove the smallest value, return
    // the diminished queue
    remove_min: function (q) { }

  };
};
```
