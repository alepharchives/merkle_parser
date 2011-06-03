merkle_parser
====================

The merkle_parser module offers capability to compare and inspect Merkle tree data structures marshaled from a 
<a href="https://github.com/andrewoswald/jmerkle_sequential" target="_blank">JMerkle</a> process.  Other producers 
can be utilized assuming they adhere to the established binary protocol.  In order to avoid <i>any</i> unmarshaling 
overhead, the merkle_parser module uses bitstring matching against raw binary.

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
The diff/2 function will yield a list of leaves that differ between the two trees.

* Inspecting a merkle tree's contents is done via 

```erlang
merkle_parser:allkeys(Tree1).
```  
The allkeys/1 function will yield a list of all leaves of the given tree.

Examples
--------

Here's a rudimentary example that inspects and compares trees marshaled from the 
<a href="https://github.com/andrewoswald/jmerkle_sequential" target="_blank">jmerkle_sequential</a> example:

```erlang
-module(merkle_parser_demo).

-export([demo/0]).

%% Shows example usage on marshaled binary from the jmerkle_sequential example.
demo() ->
    
    % the following file:read_file/1 operations assume java.io.tmpdir was simply "/tmp":
    {ok, InitialInventoryTree} = file:read_file("/tmp/InitialInventoryTree.out"),
    io:format("InitialInventoryTree: ~p~n", [InitialInventoryTree]),

    InitialInventoryKeys = merkle_parser:allkeys(InitialInventoryTree),
    io:format("InitialInventoryKeys: ~p~n", [InitialInventoryKeys]),

    {ok, AlteredInventoryTree} = file:read_file("/tmp/AlteredInventoryTree.out"),
    io:format("AlteredInventoryTree: ~p~n", [AlteredInventoryTree]),

    AlteredInventoryKeys = merkle_parser:allkeys(AlteredInventoryTree),
    io:format("AlteredInventoryKeys: ~p~n", [AlteredInventoryKeys]),

    Diff = merkle_parser:diff(InitialInventoryTree, AlteredInventoryTree),
    io:format("Diff: ~p~n", [Diff]),

    ok.
```

The output from the io:format calls:

```erlang
1> merkle_parser_demo:demo().
InitialInventoryTree: <<1,253,3,149,159,112,200,219,103,87,197,189,236,126,39,
                        194,184,136,192,17,134,0,0,0,192,0,5,184,0,91,168,142,
                        115,116,250,160,43,55,157,69,240,195,123,163,66,13,135,
                        66,198,0,0,0,7,119,105,100,103,101,116,48,16,0,165,232,
                        105,38,73,118,27,250,89,0,163,222,14,106,107,9,188,13,
                        199,63,0,0,0,7,119,105,100,103,101,116,51,43,0,131,224,
                        108,116,199,112,9,229,16,231,61,140,83,214,75,226,225,
                        96,10,113,0,0,0,7,119,105,100,103,101,116,49,84,0,203,
                        246,235,17,66,207,68,121,46,118,168,110,13,50,253,137,
                        249,73,53,169,0,0,0,7,119,105,100,103,101,116,50,112,0,
                        199,29,153,252,230,86,141,153,94,25,207,220,29,137,8,
                        146,179,152,228,112,0,0,0,7,119,105,100,103,101,116,52>>
InitialInventoryKeys: [<<"widget4">>,<<"widget2">>,<<"widget1">>,
                       <<"widget3">>,<<"widget0">>]
AlteredInventoryTree: <<1,208,246,170,194,137,215,193,114,189,228,122,86,10,
                        216,132,0,73,82,125,13,0,0,0,195,0,5,184,0,91,168,142,
                        115,116,250,160,43,55,157,69,240,195,123,163,66,13,135,
                        66,198,0,0,0,7,119,105,100,103,101,116,48,16,0,165,232,
                        105,38,73,118,27,250,89,0,163,222,14,106,107,9,188,13,
                        199,63,0,0,0,7,119,105,100,103,101,116,51,43,0,131,224,
                        108,116,199,112,9,229,16,231,61,140,83,214,75,226,225,
                        96,10,113,0,0,0,7,119,105,100,103,101,116,49,84,0,131,
                        224,108,116,199,112,9,229,16,231,61,140,83,214,75,226,
                        225,96,10,113,0,0,0,7,119,105,100,103,101,116,50,118,0,
                        234,85,245,122,169,76,116,169,44,122,147,178,86,137,0,
                        193,167,86,84,0,0,0,0,10,98,114,97,110,100,32,110,101,
                        119,33>>
AlteredInventoryKeys: [<<"brand new!">>,<<"widget2">>,<<"widget1">>,
                       <<"widget3">>,<<"widget0">>]
Diff: [<<"brand new!">>,<<"widget4">>,<<"widget2">>]
ok
```