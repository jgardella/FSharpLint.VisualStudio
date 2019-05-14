namespace FSharpLint.VisualStudio.FSharp.Linting

open System
open System.IO
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging
open FSharpLint.VisualStudio.Utils
open FSharpLint.Application
open FSharpLint.Framework.Configuration
open FSharpLint.VisualStudio
open FSharpLint.VisualStudio.FSharp.ProjectSystem
open FSharp.Compiler
open FSharpLint.Application.ConfigurationManagement
open FSharpLint.Application.ConfigurationManager

type LintTag(tooltip) = 
    inherit ErrorTag(Constants.LintTagErrorType, tooltip)

type LintTagger(doc: ITextDocument,
                vsLanguageService: VSLanguageService, 
                serviceProvider: IServiceProvider,
                projectFactory: ProjectFactory,
                openDocumentsTracker: IOpenDocumentsTracker) as self =
    let tagsChanged = Event<_, _>()
    let mutable wordSpans = []
    let buffer = doc.TextBuffer

    let dte = serviceProvider.GetDte()
    let project() = projectFactory.CreateForDocument buffer doc.FilePath
    let config =
        match ConfigurationManagement.loadConfigurationForProject doc.FilePath with
        | ConfigurationResult.Success config ->
            Some config
        | ConfigurationResult.Failure _ ->
            None
                            
    let updateAtCaretPosition (CallInUIContext callInUIContext) =
        asyncMaybe {
            let! project = project()
            let! checkFileResults = vsLanguageService.ParseAndCheckFileInProject (doc.FilePath, project)
            let! ast = checkFileResults.ParseTree
            let! source = openDocumentsTracker.TryGetDocumentText doc.FilePath
            let shouldFileBeIgnored =
                config
                |> Option.bind (fun config -> config.ignoreFiles)
                |> Option.map (fun ignoreFiles ->
                    let parsedIgnoreFiles = ignoreFiles |> Array.map IgnoreFiles.parseIgnorePath |> List.ofArray
                    IgnoreFiles.shouldFileBeIgnored parsedIgnoreFiles doc.FilePath)
                |> Option.defaultValue false

            if not shouldFileBeIgnored then
                let res = 
                    Lint.lintParsedFile
                        { Lint.OptionalLintParameters.Default with Configuration = config }
                        { Ast = ast
                          Source = source
                          TypeCheckResults = checkFileResults.CheckResults }
                        doc.FilePath

                return
                    match res with
                    | LintResult.Success warnings ->
                        warnings 
                        |> Seq.choose (fun warn ->
                            let r = warn.Range
                            let endCol =
                                if r.StartLine = r.EndLine then
                                    r.EndColumn
                                else
                                    buffer.CurrentSnapshot.GetLineFromLineNumber(r.StartLine - 1).Length
                            let range =
                                Range.mkRange "" 
                                    (Range.mkPos r.StartLine r.StartColumn)
                                    (Range.mkPos r.StartLine endCol)

                            fromFSharpRange buffer.CurrentSnapshot range
                            |> Option.map (fun span -> warn, span))
                        |> Seq.toList
                    | LintResult.Failure _ -> []
                else
                    return []
        }
        |> Async.bind (fun spans -> 
            async {
                let spans = spans |> Option.getOrElse []
                wordSpans <- spans
                let span = buffer.CurrentSnapshot.FullSpan
                do! callInUIContext <| fun _ -> tagsChanged.Trigger(self, SnapshotSpanEventArgs span)
            })

    let docEventListener = new DocumentEventListener ([ViewChange.bufferEvent buffer], 500us, updateAtCaretPosition)

    let getTags (spans: NormalizedSnapshotSpanCollection): ITagSpan<LintTag> list = 
        [
            match wordSpans with
            | [] -> ()
            | _ :: _ ->
                let currentSnapshot = spans.[0].Snapshot
                let wordSpans = 
                    if currentSnapshot = (snd wordSpans.[0]).Snapshot then
                        wordSpans
                    else
                        // If the requested snapshot isn't the same as the one our words are on, translate our spans
                        // to the expected snapshot
                        wordSpans |> List.map (fun (warn, span) -> warn, span.TranslateTo(currentSnapshot, SpanTrackingMode.EdgeExclusive))
                
                for warn, span in wordSpans do
                    if spans.OverlapsWith span then
                        yield upcast TagSpan<LintTag>(span, LintTag(warn.Info))
        ]

    interface ITagger<LintTag> with
        member __.GetTags spans =
            upcast (protectOrDefault (fun _ -> getTags spans) [])
        
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish

    interface IDisposable with
        member __.Dispose() = 
            (docEventListener :> IDisposable).Dispose()
