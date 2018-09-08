# LLAST


Description
---------------

LLAST provides multiple level representations of LLVM.

Usually abstract syntax trees constructed by users in the form of algebraic data types are transformed to LLVM IR.

Usage
---------

See `LLAST/Entry.fs`, you can run it at `$projectDir/LLAST` with command `dotnet run --project LLAST.fsproj`.

The test scripts are generated into `./ir-snippets`, and you can run them with `python run_tests.py`.

High Level Language Constructs
---------------------------------------

A set of high level constructs are located at `LLAST/Constructs`.




