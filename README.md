dijkstra-lang-llvm
==================

A Dijkstra-to-LLVM-bitcode compiler written in Haskell

Overview
--------

This compiler (written in Haskell) compiles a version of the Dijkstra programming language (see below) into
LLVM IR or LLVM bitcode.  The resulting bitcode may be run with `lli`, and the IR may be
compiled to assembly, which can then be compiled using your compiler of choice into a system
executable

Requirements
------------

The following (additional) Haskell packages are required (in addition to the base libraries):

* Parsec (Text.ParserCombinators.Parsec) 
* llvm (LLVM) -- ~~modified (see below)~~

### Modifications to LLVM/Not Using LLVM ###

~~The compiler uses a couple modifications to the LLVM (Haskell) libraries.  The code used merely 
requires a couple of methods to be made public, and a couple lines of Haskell FFI to be written.~~

If you do not wish to install the LLVM dev libraries ~~or do not wish to use the modifications,~~
you can compile as `ghc --make dcc-simple` and run as `dcc-simple INPUTFILE`.  The compiler 
will still be able to output LLVM IR, just not the LLVM bitcode.

~~Otherwise, you can install the Haskell LLVM libraries from my fork of the official repository
at https://github.com/DirectXMan12/llvm instead, until the requisite changes get merged into
the main Haskell LLVM library.~~  The latest Haskell LLVM libraries can be used.  Make sure to use
the latest sources from the github repository at https://github.com/bos/llvm (feature introduced in 
bos/llvm@3dc9717a0c23a95002bd22fffe29e7909c387f0d)


Dijkstra Language
-----------------

The Dijkstra language implemented by this compiler is roughly equivalent to Base Dijkstra as
defined by Gary Pollice.  The most notable difference is that variables must be explicitly
declared with types.

Building
--------

Building simply requires running `ghc-make dcc`

Usage
-----

    Usage dcc [OPTION...] inputfile
      -h, -?          --help                    Show this help
      -l, -j          --emit-llvm, --emit-ir    Output the compiled LLVM IR to inputfile.ll
      -p PACKAGENAME  --package=PACKAGENAME     Currently does nothing, since packages don't mean anything to LLVM
      -s              --emit-symbol-table       Output the symbol table to inputfile.st
      -r              --emit-tree, --emit-ast   Output the intial AST to inputfile.tr
      -t              --emit-taf                Output the final Three Address Form SSA code to inputfile.taf
      -v              --verbose                 Print extra compilation details
      -b              --emit-bitcode            Emit LLVM bitcode (requires an llvm installation) using `llvm-link` -- outputs to inputfile.llb
      -c              --compile, --emit-binary  Emit a system appropriate binary executable (requires an LLVM installation and a linker aliased as `ld`) -- outputs to inputfile
      -r              --run-program             Does not emit anything by itself.  Instead, runs the program bitcode on llvm
      -o FILENAME     --output=FILENAME         Set the output file name to OUTPUTNAME instead of inputfile



