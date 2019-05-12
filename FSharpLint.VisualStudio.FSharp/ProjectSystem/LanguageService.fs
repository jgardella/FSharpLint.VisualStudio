namespace FSharpLint.VisualStudio.FSharp.ProjectSystem

open System
open System.IO
open System.Diagnostics
open FSharp.Compiler.SourceCodeServices
open AsyncMaybe
open FSharpLint.VisualStudio

// --------------------------------------------------------------------------------------
/// Wraps the result of type-checking and provides methods for implementing
/// various IntelliSense functions (such as completion & tool tips). Provides default
/// empty/negative results if information is missing.
type ParseAndCheckResults private (infoOpt: (FSharpCheckFileResults * FSharpParseFileResults) option) =
    new (checkResults, parseResults) = ParseAndCheckResults(Some (checkResults, parseResults))
    static member Empty = ParseAndCheckResults None

    member __.GetSymbolUseAtLocation(line, colAtEndOfNames, lineStr, identIsland) =
        asyncMaybe {
            let! checkResults, _ = infoOpt 
            return! checkResults.GetSymbolUseAtLocation(line, colAtEndOfNames, lineStr, identIsland)
        }

    member __.GetUsesOfSymbolInFile(symbol) =
        async {
            match infoOpt with 
            | None -> return [||]
            | Some (checkResults, _parseResults) -> 
                return! checkResults.GetUsesOfSymbolInFile symbol
        }

    member __.GetAllUsesOfAllSymbolsInFile() =
        async {
            match infoOpt with
            | None -> return [||]
            | Some (checkResults, _) -> 
                return! checkResults.GetAllUsesOfAllSymbolsInFile()
        }

    member __.ParseTree = infoOpt |> Option.bind (fun (_, parseResults) -> parseResults.ParseTree)
    member __.CheckResults = infoOpt |> Option.map (fun (checkResults, _) -> checkResults)
    member __.CheckErrors = infoOpt |> Option.map (fun (checkResults, _) -> checkResults.Errors)
    member __.ParseErrors = infoOpt |> Option.map (fun (_, parseResults) -> parseResults.Errors)
    
    member x.Errors =
        x.ParseErrors 
        |> Option.getOrElse [||]
        |> Array.append (x.CheckErrors |> Option.getOrElse [||])

    member __.GetFormatSpecifierLocationsAndArity() =
        infoOpt |> Option.map (fun (checkResults, _) -> checkResults.GetFormatSpecifierLocationsAndArity())

    member __.PartialAssemblySignature =
        infoOpt |> Option.map (fun (checkResults, _) -> checkResults.PartialAssemblySignature)

    member __.ProjectContext =
        infoOpt |> Option.map (fun (checkResults, _) -> checkResults.ProjectContext)

    member x.GetUsesOfSymbolInFileAtLocation (line, col, lineStr, ident) =
        asyncMaybe {
            let! symbolUse = x.GetSymbolUseAtLocation(line + 1, col, lineStr, [ident]) 
            let! refs = x.GetUsesOfSymbolInFile(symbolUse.Symbol) |> liftAsync
            return symbolUse.Symbol, ident, refs
        }

    member __.GetIdentTooltip (line, colAtEndOfNames, lineText, names) =
        Debug.Assert(names <> [], "The names should not be empty (for which GetToolTip raises exceptions).")
        asyncMaybe {
            let! checkResults, _ = infoOpt 
            let tokenTag = FSharpTokenTag.IDENT
            return! 
                checkResults.GetToolTipText(line, colAtEndOfNames, lineText, names, tokenTag) |> liftAsync
        }

[<RequireQualifiedAccess>]
type AllowStaleResults = 
    /// Allow checker results where the source doesn't even match
    | MatchingFileName
    /// Allow checker results where the source matches but where the background builder may not have caught up yet after some other change
    /// (such as a saved change in an earlier file in the compilation order, or a saved change in a project or DLL this project depends on).
    ///
    /// This gives good, fast, accurate results for repeated requests to the same file text. Semantic responsiveness will be degraded
    /// during edition of the file.
    | MatchingSource
    /// Don't allow stale results. This waits for all background changes relevant to the file to propagate, and forces a recheck of the file text
    /// regardless of whether if has been recently checked or not.
    | No

open FSharp.Compiler.AbstractIL.Internal.Library
open System.Collections.Concurrent

type private FileState =
    | Checked
    | NeedChecking
    | BeingChecked
    | Cancelled

// --------------------------------------------------------------------------------------
// Language service 

/// Provides functionality for working with the F# interactive checker running in background
type LanguageService (?backgroundCompilation: bool, ?projectCacheSize: int, ?fileSystem: IFileSystem) =

  do Option.iter (fun fs -> Shim.FileSystem <- fs) fileSystem
  let mutable errorHandler = None
  
  let handleCriticalErrors e file source opts = 
      errorHandler |> Option.iter (fun handle -> handle e file source opts)

  // Create an instance of interactive checker.
  let checkerInstance = 
    FSharpChecker.Create(
        projectCacheSize = defaultArg projectCacheSize 50, 
        keepAllBackgroundResolutions = false,
        keepAssemblyContents = false,
        ImplicitlyStartBackgroundWork = defaultArg backgroundCompilation true)
  
  let checkerAsync (f: FSharpChecker -> Async<'a>) = 
    let ctx = System.Threading.SynchronizationContext.Current
    async {
        do! Async.SwitchToThreadPool()
        let! result = f checkerInstance
        do! Async.SwitchToContext ctx
        return result
    }

  /// When creating new script file on Mac, the filename we get sometimes 
  /// has a name //foo.fsx, and as a result 'Path.GetFullPath' throws in the F#
  /// language service - this fixes the issue by inventing nicer file name.
  let fixFileName path = 
    if (try Path.GetFullPath path |> ignore; true with _ -> false) then path
    else 
        match Environment.OSVersion.Platform with
        | PlatformID.Unix 
        | PlatformID.MacOSX -> Environment.GetEnvironmentVariable "HOME"
        | _ -> Environment.ExpandEnvironmentVariables "%HOMEDRIVE%%HOMEPATH%"
        </> Path.GetFileName path

  let files = ConcurrentDictionary<string, FileState>()
  
  let parseAndCheckFileInProject(filePath, source, options) =
      async { 
          debug "[LanguageService] ParseAndCheckFileInProject - enter"
          let fixedFilePath = fixFileName filePath
          let! res = Async.Catch (checkerAsync <| fun x -> async {
              try
                   // wait until the previous checking completed
                   while files.ContainsKey filePath &&
                         (not (files.TryUpdate (filePath, BeingChecked, Checked)
                               || files.TryUpdate (filePath, BeingChecked, NeedChecking))) do
                       do! Async.Sleep 20
                   
                   debug "[LanguageService] Change state for %s to `BeingChecked`" filePath
                   debug "[LanguageService] Parse and typecheck source..."
                   return! x.ParseAndCheckFileInProject (fixedFilePath, 0, source, options)
              finally 
                   if files.TryUpdate (filePath, Checked, BeingChecked) then
                       debug "[LanguageService] %s: BeingChecked => Checked" filePath
                   elif files.TryUpdate (filePath, Checked, Cancelled) then
                       debug "[LanguageService] %s: Cancelled => Checked" filePath })

          debug "[LanguageService]: Parse completed"
          // Construct new typed parse result if the task succeeded
          let results = 
              match res with
              | Choice1Of2 (parseResults, FSharpCheckFileAnswer.Succeeded checkResults) ->
                  // Handle errors on the GUI thread
                  debug "[LanguageService] ParseAndCheckFileInProject - HasFullTypeCheckInfo? %b" checkResults.HasFullTypeCheckInfo
                  debug "[LanguageService] ParseAndCheckFileInProject - Errors? %A" checkResults.Errors
                  ParseAndCheckResults(checkResults, parseResults)
              | Choice1Of2 (_, FSharpCheckFileAnswer.Aborted) ->
                  debug "[LanguageService] ParseAndCheckFileInProject - Aborted"
                  ParseAndCheckResults.Empty
              | Choice2Of2 e -> 
                  fail "[LanguageService] Unexpected type checking errors occurred for '%s' with %A" fixedFilePath options
                  fail "[LanguageService] Calling checker.ParseAndCheckFileInProject failed: %A" e
                  debug "[LanguageService] Type checking fails for '%s' with content=%A and %A.\nResulting exception: %A" fixedFilePath source options e
                  handleCriticalErrors e fixedFilePath source options
                  ParseAndCheckResults.Empty
          return results
      }

  member __.OnFileChanged filePath = 
    files.AddOrUpdate (filePath, NeedChecking, (fun _ oldState -> 
        match oldState with
        | BeingChecked -> Cancelled
        | Cancelled -> Cancelled
        | NeedChecking -> NeedChecking
        | Checked -> NeedChecking))
    |> debug "[LanguageService] %s changed: set status to %A" filePath

  member __.OnFileClosed filePath = 
    match files.TryRemove filePath with
    | true, _ -> debug "[LanguageService] %s was removed from `files` dictionary" filePath
    | _ -> ()

  /// Constructs options for the interactive checker for the given file in the project under the given configuration.
  member x.GetCheckerOptions(fileName, projFilename, source, files, args, referencedProjects, fscVersion) =
    let ext = Path.GetExtension(fileName)
    let opts = 
        if ext = ".fsx" || ext = ".fsscript" then
           // We are in a stand-alone file or we are in a project, but currently editing a script file
           x.GetScriptCheckerOptions(fileName, projFilename, source, fscVersion)
          
        // We are in a project - construct options using current properties
        else async { return x.GetProjectCheckerOptions(projFilename, files, args, referencedProjects) }
    opts

  /// Constructs options for the interactive checker for the given script file in the project under the given configuration. 
  member __.GetScriptCheckerOptions(fileName, projFilename, source, fscVersion) =
      async {
        // We are in a stand-alone file or we are in a project, but currently editing a script file
        try 
            let fileName = fixFileName(fileName)
            debug "GetScriptCheckerOptions: Creating for stand-alone file or script: '%s'" fileName
            let! opts = checkerInstance.GetProjectOptionsFromScript(fileName, source, fakeDateTimeRepresentingTimeLoaded projFilename)
            let opts = fst opts
                
            let results =
                // The FSharpChecker resolution sometimes doesn't include FSharp.Core and other essential assemblies, so we need to include them by hand
                if opts.OtherOptions |> Seq.exists (fun s -> s.Contains("FSharp.Core.dll")) then
                    match fscVersion with
                    | FSharpCompilerVersion.FSharp_3_0
                    | FSharpCompilerVersion.FSharp_3_1 ->
                        let dirs = FSharpEnvironment.getDefaultDirectories(fscVersion, FSharpTargetFramework.NET_4_5)
                        FSharpEnvironment.resolveAssembly dirs "FSharp.Core"
                        |> Option.map (fun path -> 
                            let fsharpCoreRef = sprintf "-r:%s" path
                            { opts with OtherOptions = [| yield fsharpCoreRef
                                                          yield! opts.OtherOptions |> Seq.filter (fun s -> not (s.Contains "FSharp.Core.dll")) |] })
                        |> Option.getOrElse opts
                    | _ -> opts
                else 
                // Add assemblies that may be missing in the standard assembly resolution
                debug "GetScriptCheckerOptions: Adding missing core assemblies."
                let dirs = FSharpEnvironment.getDefaultDirectories(fscVersion, FSharpTargetFramework.NET_4_5)
                { opts with OtherOptions =  [|  yield! opts.OtherOptions
                                                match FSharpEnvironment.resolveAssembly dirs "FSharp.Core" with
                                                | Some fn -> yield sprintf "-r:%s" fn
                                                | None -> debug "Resolution: FSharp.Core assembly resolution failed!"
                                                match FSharpEnvironment.resolveAssembly dirs "FSharp.Compiler.Interactive.Settings" with
                                                | Some fn -> yield sprintf "-r:%s" fn
                                                | None -> debug "Resolution: FSharp.Compiler.Interactive.Settings assembly resolution failed!" |] }
              
            // Print contents of check option for debugging purposes
            debug "GetScriptCheckerOptions: ProjectFileName: %s, FSharpProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A" 
                                    results.ProjectFileName results.OtherOptions results.IsIncompleteTypeCheckEnvironment results.UseScriptResolutionRules
            return results
        with e -> 
            return failwithf "Exception when getting check options for '%s'\n.Details: %A" fileName e
      }
  
  /// Constructs options for the interactive checker for a project under the given configuration. 
  member __.GetProjectCheckerOptions(projFilename, files, args, referencedProjects) =
    let opts =
        { ProjectFileName = projFilename
          ProjectId = None
          SourceFiles = files
          OriginalLoadReferences = List.empty
          ExtraProjectInfo = None
          Stamp = None
          OtherOptions = args
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = false
          LoadTime = fakeDateTimeRepresentingTimeLoaded projFilename
          UnresolvedReferences = None
          ReferencedProjects = referencedProjects }
    debug "GetProjectCheckerOptions: ProjectFileName: %s, FSharpProjectOptions: %A, IsIncompleteTypeCheckEnvironment: %A, UseScriptResolutionRules: %A, ReferencedProjects: %A" 
                                    opts.ProjectFileName opts.OtherOptions opts.IsIncompleteTypeCheckEnvironment opts.UseScriptResolutionRules opts.ReferencedProjects
    opts

  member __.ParseFileInProject(projectOptions, fileName: string, src) = 
    async {
        debug "Parsing: Get untyped parse result (fileName=%s)" fileName
        return! checkerAsync (fun x -> x.ParseFileInProject(fileName, src, projectOptions))
    }

  member internal __.TryGetStaleTypedParseResult(fileName:string, options, src, stale)  = 
    // Try to get recent results from the F# service
    let res = 
        match stale with 
        | AllowStaleResults.MatchingFileName -> checkerInstance.TryGetRecentCheckResultsForFile(fileName, options) 
        | AllowStaleResults.MatchingSource -> checkerInstance.TryGetRecentCheckResultsForFile(fileName, options, source=src)
        | AllowStaleResults.No -> None
    match res with 
    | Some (untyped,typed,_) when typed.HasFullTypeCheckInfo  -> Some (ParseAndCheckResults(typed, untyped))
    | _ -> None

  /// Parses and checks the given file in the given project under the given configuration. Asynchronously
  /// returns the results of checking the file.
  member x.ParseAndCheckFileInProject(opts, fileName: string, src, stale) = 
      async { 
          match x.TryGetStaleTypedParseResult(fileName, opts, src, stale) with
          | Some results -> return results
          | None -> 
              debug "Parsing: Trigger parse (fileName=%s)" fileName
              return! parseAndCheckFileInProject(fileName, src, opts)
      }

  member __.InvalidateConfiguration options =
      checkerAsync <| fun checker -> async { checker.InvalidateConfiguration options }

  member __.RawChecker = checkerInstance

    /// Get all the uses in the project of a symbol in the given file (using 'source' as the source for the file)
    member __.IsSymbolUsedInProjects(symbol: FSharpSymbol, currentProjectName: string, projectsOptions: FSharpProjectOptions seq) =
        projectsOptions
        |> Seq.toArray
        |> Async.Array.exists (fun opts ->
            async {
                let! projectResults = checkerAsync (fun x -> x.ParseAndCheckProject opts)
                let! refs = projectResults.GetUsesOfSymbol symbol
                return
                    if opts.ProjectFileName = currentProjectName then refs.Length > 1
                    else refs.Length > 0 })

    member x.GetIdentTooltip (line, colAtEndOfNames, lineStr, names, project: FSharpProjectOptions, file, source) =
        async {
            let! checkResults = x.ParseAndCheckFileInProject (project, file, source, AllowStaleResults.No)
            return! checkResults.GetIdentTooltip (line, colAtEndOfNames, lineStr, names)
        }

    member __.SetCriticalErrorHandler func = errorHandler <- Some func