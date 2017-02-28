# makefile - A Library and Tool for Generating Makefiles for Curry Applications

This package contains a library `MakeFile` with datatypes
to describe the structure of makefiles and print them
in the standard makefile syntax.
Furthermore, the package also contains a tool to generate
a makefile for a Curry application.

## Installing the tool

After installing and checking out the tool by the command

    > cpm checkout makefile

go into the root directory of the package and run

    > cpm install

This installs the executable `curry-genmake' in the bin directory
of CPM.


## Using the tool

In the simplest case, run the executable with the main module of
the application as an argument, e.g.,

    > curry-genmake Main

This should output a makefile. There are further options to write
the output into a file or provide some name to which the main executable
will be linked when installing the application with the makefile
(use option `--help`).

If the application needs a specific load path for modules,
this load path must be set via the environment variable `CURRYPATH`.
For instance, to generate a makefile for some package managed with CPM,
one can use the CPM command `cpm exec` inside the package directory,
e.g.,

    > cpm exec curry-genmake Main
