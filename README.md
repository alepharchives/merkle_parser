merkle_parser
====================

The merkle_parser module offers capability to compare and inspect Merkle tree data structures marshaled from a 
<a href="https://github.com/andrewoswald/jmerkle_sequential" target="_blank">JMerkle</a> process.  Other producers 
can be utilized assuming they adhere to the established binary protocol.  In order to avoid <i>any</i> unmarshaling 
overhead, the merkle_parser module processes raw binary.

While this module's out-of-box Merkle tree parsing/comparison performance is already extremely fast, HiPE enabled
environments will experience yet <i>even better</i> performance.

Getting Started
---------------

The merkle_parser module need only be compiled in order to start using it.
    

Usage
-----
The compare and inspect operations are exported as diff/2 and allkeys/1 respectively.

* Comparing two merkle tree structures, Tree1 & Tree2, is done via
```erlang
merkle_parser:diff(Tree1,Tree2).
```

* Inspecting a merkle tree's contents is done via 

```erlang
merkle_parser:allkeys(Tree1).
```

Examples
--------

Here's a rudimentary example that inspects and compares trees marshaled from the jmerkle_sequential example:

```erlang
-module(hello_merkle_parser).

%%TODO.
```