This directory contains templates for macOS and linux which you can use to start your VeriFx project.
The templates are identical, except that the macOS template includes the required `dylib` files for Z3,
whereas the linux template includes the required `so` files.
I've not created a template for Windows because I don't have a Windows machine to test on,
but it should be as simple as taking one of the existing templates and replacing the library files with the `dll` files for Z3 (version 4.8.14.0).
