# Ape
Ape is a minimalistic modal text editor developed in F# programming language.

It was strongly inspired by Vim and Kakoune text editors.

It's intended to be used as a Swiss knife for structural text editing and analyzing log or trace files. It's not a replacement for IDE or any established text editor you might use today.

## Highlights
- Console oriented
- Normal mode and Insert mode for text editing
- PromptInsert mode and PromptNormal mode for prompt editing
- Multiple cursors/selections
- Many operations on the selected text and the current cursors/selections
- Undo/Redo, named Undo/Redo
- Storing and loading of cursors/selections
- Multiple registers, a special register for OS clipboard
- Command prompt with its own history
- Multiple regex prompts with shared history - "search", "extract", "select" and "keep"
- Single-line and multiple-lines regex search
- Coloring of named groups for "search"
- Simple word (identificator) completion in Insert mode and regex prompts
- Command and command arguments completion in command prompt
- Macro recording and editing
- Light and dark color scheme

## No-goals
- Mouse support
- Syntax highlighting for programming languages
- Text completion for programming languages
- Scripting

## Building
Ape can be built for Windows and Linux using Visual Studio.

For the best experience on Windows, Ape should be run in Windows Terminal.

On Linux, you will probably have to remap all key bindings with Ctrl modifier to something else as Linux console practically doesn't support key bindings with Ctrl modifier.

You can get a single Ape executable file accompanied by the basic configuration files and the help file by publishing the Ape project in Visual Studio, with Deployment mode "Self-contained", and File publish options "Produce single file" and "Trim unused code".

## Integrated help
Press F1 in Normal mode or run :help command (or :h) for help. To get back into Normal mode from wherever you are, press Esc.

Happy experimenting!
