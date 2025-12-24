# Ape
Ape is a minimalistic modal text editor developed in the F# programming language.

It was strongly inspired by the Vim and Kakoune text editors.

It's intended to be used as a Swiss knife for structural text editing and log or trace files analysis. It's not a replacement for an IDE or any established text editor you might use today.

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
- Multiple regex prompts with a shared history - "search", "extract", "select", "keep" and "discard"
- Single-line and multiple-lines regex search
- Coloring of named groups for "search" and "extract"
- Simple word (identificator) completion in Insert mode and regex prompts
- Command name and command arguments completion in command prompt
- Macro recording and editing
- Light and dark color schemes

## Non-goals
- Mouse support
- Syntax highlighting
- Context sensitive text completion
- Scripting

## Building
Ape can be built for Windows and Linux using Visual Studio (on Windows).

It can also be built from the command line on Windows and Linux, provided that you have [.NET SDK 10.0](https://dotnet.microsoft.com/en-us/download/dotnet/10.0) or [.NET SDK 9.0](https://dotnet.microsoft.com/en-us/download/dotnet/9.0) installed on your PC:

`dotnet build Ape/Ape.fsproj`

To run the tests:

`dotnet test ApeTest/ApeTest.fsproj`

You can get a single Ape executable file accompanied by the basic configuration files and the help file by publishing the Ape project in Visual Studio, with Deployment mode "Self-contained", and File publish options "Produce single file" and "Trim unused code". With .NET SDK 10.0, File publish option "Enable ReadyToRun compilation" works too.

The same can be achieved from the command line for win-x64 binary:

`dotnet publish -r win-x64 --sc true -p:PublishSingleFile=true -p:PublishTrimmed=true -p:PublishReadyToRun=true Ape/Ape.fsproj`

For linux-x64 binary:

`dotnet publish -r linux-x64 --sc true -p:PublishSingleFile=true -p:PublishTrimmed=true -p:PublishReadyToRun=true Ape/Ape.fsproj`

For the best experience on Windows, Ape should be run in Windows Terminal.

On Linux, you will probably have to remap all key bindings with Ctrl modifier to something else as Linux console practically doesn't support key bindings with Ctrl modifier.

OSX support was not tested yet.

## Windows Defender false positives
From version 0.16 up, Ape may be detected by Windows Defender as a trojan. It's probably caused by the recently added #filePath completion functionality, which uses IO.Directory.EnumerateFileSystemEntries method to get the list of completion candidates.

## Integrated help
Press F1 in Normal mode or run :help command (or :h) for help. To get back into Normal mode from wherever you are, press Esc.

Happy experimenting!
