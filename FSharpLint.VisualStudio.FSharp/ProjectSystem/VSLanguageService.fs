namespace FSharpLint.VisualStudio.FSharp.ProjectSystem

open FSharpLint.VisualStudio.Utils
open FSharpLint.VisualStudio
open FSharpLint.VisualStudio.FSharp
open FSharpLint.VisualStudio.FSharp.ProjectSystem
open AsyncMaybe
open ViewModule.Progress
open ViewModule.Progress.FSharp
open Microsoft.VisualStudio.Editor
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open System
open System.Collections.Generic
open System.IO
open System.Diagnostics
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.VisualStudio.Text.Editor

type RawEntity = 
    { /// Full entity name as it's seen in compiled code (raw FSharpEntity.FullName, FSharpValueOrFunction.FullName). 
      FullName: string
      /// Entity name parts with removed module suffixes (Ns.M1Module.M2Module.M3.entity -> Ns.M1.M2.M3.entity)
      /// and replaced compiled names with display names (FSharpEntity.DisplayName, FSharpValueOrFucntion.DisplayName).
      /// Note: *all* parts are cleaned, not the last one. 
      CleanedIdents: Idents
      Namespace: Idents option
      IsPublic: bool
      TopRequireQualifiedAccessParent: Idents option
      AutoOpenParent: Idents option
      Kind: EntityKind }
    override x.ToString() = sprintf "%A" x  

type AssemblyContentCacheEntry =
    { FileWriteTime: DateTime 
      ContentType: AssemblyContentType 
      Entities: RawEntity list }

[<NoComparison; NoEquality>]
type IAssemblyContentCache =
    abstract TryGet: AssemblyPath -> AssemblyContentCacheEntry option
    abstract Set: AssemblyPath -> AssemblyContentCacheEntry -> unit

[<RequireQualifiedAccess; NoComparison>]
type SymbolDeclarationLocation = 
    | File
    /// The case where the declared symbol may be included into several projects
    | Projects of IProjectProvider list * isLocalForProject: bool

type ShowProgress = OperationState -> unit

type EntityCache() =
    let dic = Dictionary<AssemblyPath, AssemblyContentCacheEntry>()
    interface IAssemblyContentCache with
        member __.TryGet assembly =
            match dic.TryGetValue assembly with
            | true, entry -> Some entry
            | _ -> None
        member __.Set assembly entry = dic.[assembly] <- entry

    member __.Clear() = dic.Clear()
    member x.Locking f = lock dic <| fun _ -> f (x :> IAssemblyContentCache)

[<Export>]
type VSLanguageService
    [<ImportingConstructor>] 
    (editorFactory: IVsEditorAdaptersFactoryService, 
     fsharpLanguageService: FSharpLanguageService,
     openDocumentsTracker: IOpenDocumentsTracker,
     [<Import(typeof<FileSystem>)>] fileSystem: IFileSystem,
     [<Import(typeof<SVsServiceProvider>)>] serviceProvider: IServiceProvider) =

    let instance = LanguageService ()

    /// Log exceptions to 'ActivityLog' if users run 'devenv.exe /Log'.
    /// Clean up instructions are displayed on status bar.
    let suggestRecoveryAfterFailure ex fileName _source opts =
        Logging.logError (fun _ -> sprintf "The following exception: %A occurs for file '%O' and options '%A'." ex fileName opts)
        let statusBar = serviceProvider.GetService<IVsStatusbar, SVsStatusbar>()
        statusBar.SetText "Error in VSLanguageService" |> ignore 
                
    do instance.SetCriticalErrorHandler suggestRecoveryAfterFailure
       openDocumentsTracker.DocumentChanged.Add instance.OnFileChanged
       openDocumentsTracker.DocumentClosed.Add instance.OnFileClosed

    let entityCache = EntityCache()

    member __.ParseFileInProject (fileName, projectProvider: IProjectProvider) =
        asyncMaybe {
            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
            let! source = openDocumentsTracker.TryGetDocumentText fileName
            return! instance.ParseFileInProject(opts, fileName, source) |> liftAsync
        }

    member __.ParseFileInProject (fileName, source, projectProvider: IProjectProvider) =
        async {
            let! opts = projectProvider.GetProjectCheckerOptions instance
            return! instance.ParseFileInProject(opts, fileName, source)
        }

    member __.ParseAndCheckFileInProject (currentFile: string, projectProvider: IProjectProvider, ?allowStaleResults) =
        let allowStaleResults = defaultArg allowStaleResults AllowStaleResults.No
        asyncMaybe {
            let! opts = projectProvider.GetProjectCheckerOptions instance |> liftAsync
            let! source = openDocumentsTracker.TryGetDocumentText currentFile
            return! instance.ParseAndCheckFileInProject(opts, currentFile, source, allowStaleResults) |> liftAsync
        }

    member __.GetProjectCheckerOptions (project: IProjectProvider) = project.GetProjectCheckerOptions instance

    member __.InvalidateProject (projectProvider: IProjectProvider) = 
        async {
            let! opts = projectProvider.GetProjectCheckerOptions(instance) 
            return! instance.InvalidateConfiguration opts
        }

    member __.ClearCaches() = 
        debug "[Language Service] Clearing FCS caches."
        instance.RawChecker.InvalidateAll()
        entityCache.Clear()
    
    member __.CheckProjectInBackground (opts: FSharpProjectOptions) =
        debug "[LanguageService] StartBackgroundCompile (%s)" opts.ProjectFileName
        instance.RawChecker.CheckProjectInBackground opts

    member __.RawChecker = instance.RawChecker

    member __.GetCompleteTextForDocument filename =
        openDocumentsTracker.TryGetDocumentText filename

    member this.MakePointInDocument (textDocument: ITextDocument, snapshotPoint: SnapshotPoint) =
        maybe {
            let! source = this.GetCompleteTextForDocument textDocument.FilePath
            return snapshotPoint.MakePointInDocument textDocument.FilePath source
        }

    member this.MakePointInDocument (textDocument: ITextDocument, view: IWpfTextView) =
        maybe {
          let! snapshotPoint = view.SnapshotPointAtCaret
          return this.MakePointInDocument(textDocument, snapshotPoint)
        }