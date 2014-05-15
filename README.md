This project contains a simple implementation of the following:

* Regexp lexer and parser with the most common constructs
* epsilon-NFA construction from regexp using the constructive proof of equivalence
* Conversion from epsilon-NFA to DFA using the modified subset construction method
* DFA minimization using the table fill method in time O(n^2)
* Simulation of the DFA on inputs
* Simple driver accepting a language, some regexp, and a set of strings (~grep)
