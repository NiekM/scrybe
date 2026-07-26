# Scrybe

This package accompanies the PADL '23 paper ["Program Synthesis Using Example Propagation"](https://niekm.github.io/papers/padl23.pdf)

Scrybe implements component-based, type-and-example directed program synthesis using example propagation.
During synthesis, top-level example constraints are propagated through the program in a top-down fashion using [live bidirectional evaluation](https://dl.acm.org/doi/10.1145/3408991), allowing arbitrary components to be used during synthesis.
