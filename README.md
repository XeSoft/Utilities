# Utilities
General utility functions in F#.

### Agents

Based on the MailboxProcessor, these components were created to ease the boilerplate 
in creating agents to process chunks of work.

##### Agent

Functions to create, send messages to, and stop a mailbox processor.

##### AgentDistributor

Itself an agent, it takes submitted messages and distributes them off to other agents 
based on the provided hashing function. Agents are commissioned as needed (per hash value) 
and decommissioned when idle.

### CircularDictionary

This is a combination of a dictionary and a circular buffer. Created with an initial capacity.
When the capacity is reached, subsequent additions to the dictionary overwrite the oldest entry.

### CircularMap

This is a combination of a map and a circular buffer. Created with an initial capacity.
When the capacity is reached, subsequent additions cause the oldest map entry to be removed.

### Shuffling

One of the higher-level capabilities of this library is to provide several ways to shuffle a sequence.
Some of the other utilities primarily exist to support shuffling.

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