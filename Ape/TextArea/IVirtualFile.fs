module IVirtualFile

open DataTypes
open FileUtils

type IVirtualFile =
    /// Returns lines of the virtual file source.
    abstract member Read:
        filePath: string -> Lines option

    /// Writes lines to the virtual file source.
    abstract member Write:
        filePath: string -> lines: Lines -> unit option

    /// Returns state of the virtual file source.
    abstract member GetDiskFileState:
        filePath: string -> DiskFileState option

type DefaultVirtualFile() =
    interface IVirtualFile with
        member _.Read _             = None
        member _.Write _ _          = None
        member _.GetDiskFileState _ = None
