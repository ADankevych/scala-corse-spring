# Unit 5

You are challenged to implement Set for Numerals based on binary tree:
* implement `NumeralSet`, `Empty` and `NonEmpty`;
* `remove`, `difference` and `symmetric difference` operations are optional challenges;
* generators are already provided for you;
* implement the required tests in `NumeralSetSpecification`;
* all tests should pass;
* github build must be green.

## Set
```
NumeralSet ::= Empty | NonEmpty(NumeralSet, Numeral, NumeralSet)
```

Review
* [Set](https://en.wikipedia.org/wiki/Set_(mathematics))
* [Binary tree](https://en.wikipedia.org/wiki/Binary_tree)