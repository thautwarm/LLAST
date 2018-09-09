# LLAST


Description
---------------

LLAST provides multiple level representations of LLVM.

Usually abstract syntax trees constructed by users in the form of algebraic data types are transformed to LLVM IR.

Usage
---------

**For Windows users**

Use `cd LLAST && dotnet publish -c Release -r win10-x64` to create a executable which could compile a LISP dialect into LLVM IR.  

See examples at `examples/*.lisp`.

High Level Language Constructs
---------------------------------------

A set of high level constructs are located at `LLAST/Constructs`.




