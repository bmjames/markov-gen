A simple Markov chain text generator.

Given one or more input files to analyse, and an optional minimum word count, the program uses a Markov chain algorithm to produce sentences of nonsense in the literary style of the input.

Usage:

    markov [-n|--chain-length NUMBER] [-m|--min-words NUMBER] FILES...

For example,

    $ ./markov --min-words=10 monbiot*.txt
    Despite this scandal, Seafish survived the government's bonfire of the farming lobby so great that the Guardian it was cut and burnt and grazed by people and their history.
