# Utilities
General utility functions in F#.

### Shuffling

One of the higher-level capabilities of this library is to provide several ways to shuffle a sequence.
Several of the other utilities primarily exist to support shuffling.

##### Seq.shuffle

Seq.shuffle is a Durstenfeld (also called Fisher-Yates or Knuth) shuffle with o(n) complexity.
You must provide the random number generator function which generates an integer between 0 and the function argument (inclusive).

##### Seq.shuffleSeeded

Seq.shuffleSeeded uses Seq.shuffle and plugs in the .NET standard pseudo-random number generator.
The main benefit of using this is that you can specify the seed so that shuffles are reproducible.
If you provide the same seed and sequence, you will always get the same shuffled result.
Otherwise, since the PRNG is not cryptographically secure it could be vulnerable to prediction and poor distribution.

##### Seq.shuffleCrypto

Seq.shuffleCrypto uses Seq.shuffle and plugs in the .NET standard cryptographic random number generator.
The benefit is that the generated values are not easily guessed and are pretty well distributed.
However, the results are not reproducible with a seed value.
Multiple random numbers (up to the sequence size) are buffered ahead of time to save overhead.