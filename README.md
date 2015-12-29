<img src="img/logo.png" alt="logo" align="left" />

Know your algorithms cold!  Love 'm or hate 'm, algorithmic questions
reign supreme at tech interviews so take the opportunity to practice
them while sharpening your Haskell skills.

This project contains tests and solutions for the most commonly asked
questions from major tech companies -- all in Haskell. You can challenge
yourself to rewrite (and improve!) the solutions while keeping the tests
passing.

Each answer contains notes about clarifying questions to ask the
interviewer, a strategic breakdown and runtime complexity analysis.

### Two-Level Approach

There are two types of questions. One is to implement data structures
and simple operations from scratch and the other to apply and combine
known techniques to solve higher level problems. For the latter we
use the [containers](https://hackage.haskell.org/package/containers),
[unordered-containers](https://hackage.haskell.org/package/unordered-containers),
[vector](https://hackage.haskell.org/package/vector),
[matrix](https://hackage.haskell.org/package/matrix),
[KMP](https://hackage.haskell.org/package/KMP) and
[fgl](https://hackage.haskell.org/package/fgl) packages as our
arsenal to demolish problems.

### The Questions

* [Quora](https://www.reddit.com/r/cscareerquestions/comments/20ahfq/heres_a_pretty_big_list_of_programming_interview/)
  * [General](src/Inter/Quora/General.hs) |
    [Tests](test/Inter/Quora/GeneralSpec.hs)
  * [Strings](src/Inter/Quora/Strings.hs) |
    [Tests](test/Inter/Quora/StringsSpec.hs)
  * Trees
  * Stacks, Queues, and Heaps
  * Linked Lists
  * Sorting
