# Ape
Ape is a minimalistic modal text editor developed in F# programming language.

It was strongly inspired by Vim and Kakoune text editors.

It's intended to be used as a Swiss knife for structural text editing and analyzing log or trace files. It's not a replacement for IDE or any established text editor you might use today.

Ape can be built for Windows and Linux using Visual Studio.

For the best experience on Windows, Ape should be run in Windows Terminal.

On Linux, you will probably have to remap all key bindings with Ctrl modifier to something else as Linux console practically doesn't support key bindings with Ctrl modifier.

You can get a single Ape executable file accompanied by the basic configuration files and the help file by publishing the Ape project in Visual Studio, with Deployment mode "Self-contained", and File publish options "Produce single file" and "Trim unused code".

Press F1 in Normal mode or run :help command (or :h) for help. To get back into Normal mode from wherever you are, press Esc.

Happy experimenting!
