# flang minilanguage

## What is this?
This is a compiler + abstract machine for the minimal functional language F, which is defined in (1) François Bry und Norbert Eisinger: Übersetzerbau – Abstract Machines, 2004 (in German) (https://www.en.pms.ifi.lmu.de/publications/lecture-notes/uebersetzerbau/www-skriptum-2004.pdf).
The abstract machine is a slightly simplified and corrected version of the one defined in (1) accompanying the language F.
The compiler extends the procedure in (1) by adding compile-time type-checking based on a polymorphic, iterative typification algorithm.

## System requirements
It should run on any modern Linux or MacOS system that has a working Nix installation.
Windows was not tested, but it probably works, too.
If you do not have a working Nix installation, you are on your own on providing the required Haskell + Stack versions as they are derived in the nix-shell definition.

## How to use?
This project is based on Haskell + Stack, so you need to have it in your environment before you can run the compiler.
This can be accomplished by installing manually or by using Nix to derive a complete development environment automatically.
This is done with Nix by issuing 'nix-shell' in your terminal (when called in the project root).
If direnv is also installed and configured, it can do this automatically once 'direnv allow' was issued once in the project root.

When the preparations are done, one can use 'stack run executable filepath' to run the executable filepath with file filepath.
When running directly inside a nix-shell, 'stack run' can actually be omitted since bash aliases are defined for all executables.
The project defines the following executables:

| Executable             | Function                                                                                                                                          |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| flang-tokenize         | Tokenize the input program in the file and output the result                                                                                      |
| flang-tokenize-debug   | Tokenize the input program in the file and output the result along with extensive debugging information (that may be very difficult to read)      |
| flang-parse            | Parse the input program and output the result                                                                                                     |
| flang-parse-debug      | Parse the input program and output the result along with extensive debugging information (that may be very difficult to read)                     |
| flang-typify           | Typify the input program and output the result                                                                                                    |
| flang-typify-debug     | Typify the input program and output the result along with extensive debugging information (that may be very difficult to read)                    |
| flang-rewrite          | Rewrite the input program and output the result                                                                                                   |
| flang-rewrite-debug    | Rewrite the input program and output the result along with extensive debugging information (that may be very difficult to read)                   |
| flang-compile          | Compile the input program and output the result                                                                                                   |
| flang-compile-debug    | Compile the input program and output the result along with extensive debugging information (that may be very difficult to read)                   |
| flang-run              | Run the input program and output the result                                                                                                       |
| flang-run-debug        | Run the input program and output the result along with extensive debugging information (that may be very difficult to read)                       |
| flang-run-vmcode       | Run the input program given as machine code and output the result                                                                                 |
| flang-run-vmcode-debug | Run the input program given as machine code and output the result along with extensive debugging information (that may be very difficult to read) |