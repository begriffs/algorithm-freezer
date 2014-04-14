## Your challenge

Implement a hash table (implementing separate chaining might be
easiest). Fill out these functions and save the result to
`submissions/hash/[your-github-name].js`.  Run the test to see if
you got it right. `node test/hash.js`. Once your code passes send
a pull request and add your solution to the freezer!

Note that this interface requires you to use a rolling hash function
and write another function to recalculate the hash of a moving
fixed-size window.

**Note**: JavaScript's modulo operator is remainder, not a true
modulo and it handles negative values incorrectly. This might give
you problems when creating the `roll` function. Here is real mod:

```js
function mod(m, n) {
  return ((m % n) + n) % n;
}
```

### Hash table template

```js
module.exports = function () {
  return self = {

    // create an empty hash table with
    // n buckets
    empty: function (n) { },

    // hash the string s into a value h
    // where 0 ≤ h < n
    hash: function (n, s) { },

    // Recalculate the hash of a fixed-length window moved one
    // character forward in a larger string. Here n is the number of
    // buckets, oldHash is the hash of the original window position,
    // w is the string in the original window, and shiftChar is the
    // next character to enter the window on the right.
    //
    // .. [ .. w .. ] nextChar ..
    roll: function (n, oldHash, w, shiftChar) { },

    // Add value to the hash table, and
    // return the table.
    //
    // value will be a string
    insert: function (t, value) { },

    // Boolean. Does t contain value?
    find: function (t, value) { },

    // Return the table with value removed
    remove: function (t, value) { }

  };
};
```
