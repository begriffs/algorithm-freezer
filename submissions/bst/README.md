## Your challenge

Implement an unbalanced binary search tree. Fill out these functions
and save the result to `src/bst/[your-github-name].js`. Run the
test to see if you got it right. `node test/bst.js`. Once your code
passes send a pull request and add your solution to the freezer!

### BST template

```js
module.exports = function () {
  return self = {

    // create a node of the tree
    node: function (value, left, right) {
      return { value: value, left: left, right: right };
    },

    // add a value to the tree
    insert: function (t, value) { },

    // the values in the tree as a sorted array
    sorted: function (t) { },

    // the node with given value, or null
    // (I ended up adding an optional third
    //  parameter for my internal use)
    find: function (t, value) { },

    // the smallest value in the tree
    min: function (t) { },

    // the node with next greatest value
    successor: function (t, value) { },

    // remove node containing value
    remove: function (t, value) { }

  };
};
```
