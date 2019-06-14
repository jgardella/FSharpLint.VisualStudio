﻿namespace FSharpLint.VisualStudio

open System

type IGeneralOptions =
    abstract XmlDocEnabled: bool with get, set
    abstract FormattingEnabled: bool with get, set
    abstract HighlightUsageEnabled: bool with get, set
    abstract HighlightPrintfUsageEnabled: bool with get, set
    abstract RenameRefactoringEnabled: bool with get, set
    abstract DepthColorizerEnabled: bool with get, set
    abstract NavigateToEnabled: bool with get, set
    abstract SyntaxColoringEnabled: bool with get, set
    abstract InterfaceImplementationEnabled: bool with get, set
    abstract FolderOrganizationEnabled: bool with get, set
    abstract FindAllReferencesEnabled: bool with get, set
    abstract GenerateRecordStubEnabled: bool with get, set
    abstract UnionPatternMatchCaseGenerationEnabled: bool with get, set
    abstract ResolveUnopenedNamespacesEnabled: bool with get, set
    abstract UnusedReferencesEnabled: bool with get, set
    abstract UnusedOpensEnabled: bool with get, set
    abstract TaskListCommentsEnabled: bool with get, set
    abstract GoToMetadataEnabled: bool with get, set
    abstract GenerateReferencesEnabled: bool with get, set
    abstract GoToSymbolSourceEnabled: bool with get, set
    abstract QuickInfoPanelEnabled: bool with get, set
    abstract LinterEnabled: bool with get, set
    abstract OutliningEnabled: bool with get, set
    abstract PeekDefinitionEnabled: bool with get, set

type IFormattingOptions =
    abstract PageWidth: int with get, set
    abstract SemicolonAtEndOfLine: bool with get, set
    abstract SpaceBeforeArgument: bool with get, set
    abstract SpaceBeforeColon: bool with get, set
    abstract SpaceAfterComma: bool with get, set
    abstract SpaceAfterSemicolon: bool with get, set
    abstract SpaceAroundDelimiter: bool with get, set
    abstract IndentOnTryWith: bool with get, set
    abstract ReorderOpenDeclaration: bool with get, set

type CodeGenerationKinds =
    | Failwith = 0
    | NotImplementedYet = 1
    | DefaultValue = 2
    | Uncompilable = 3

type ICodeGenerationOptions =
    abstract DefaultBody: string with get, set
    abstract CodeGenerationOptions: CodeGenerationKinds with get, set
    abstract InterfaceMemberIdentifier: string with get, set
     
type IGlobalOptions =
    abstract DiagnosticMode: bool with get, set
    abstract BackgroundCompilation: bool with get, set
    abstract ProjectCacheSize: int with get, set
    abstract PeekStandaloneFilesEnabled: bool with get, set

type ILintOptions =
    abstract UpdateDirectories: unit -> unit
    abstract GetConfigurationForDirectory: string -> FSharpLint.Application.ConfigurationManager.Configuration

type IOutliningOptions =
    abstract OpensEnabled: bool with get, set
    abstract OpensCollapsedByDefault: bool with get, set
    abstract ModulesEnabled: bool with get, set
    abstract ModulesCollapsedByDefault: bool with get, set
    abstract HashDirectivesEnabled: bool with get, set
    abstract HashDirectivesCollapsedByDefault: bool with get, set
    abstract TypesEnabled: bool with get, set
    abstract TypesCollapsedByDefault: bool with get, set
    abstract SimpleTypesEnabled: bool with get, set
    abstract SimpleTypesCollapsedByDefault: bool with get, set
    abstract TypeExpressionsEnabled: bool with get, set
    abstract TypeExpressionsCollapsedByDefault: bool with get, set
    abstract MembersEnabled: bool with get, set
    abstract MembersCollapsedByDefault: bool with get, set
    abstract LetOrUseEnabled: bool with get, set
    abstract LetOrUseCollapsedByDefault: bool with get, set
    abstract CollectionsEnabled: bool with get, set
    abstract CollectionsCollapsedByDefault: bool with get, set
    abstract PatternMatchesEnabled: bool with get, set
    abstract PatternMatchesCollapsedByDefault: bool with get, set
    abstract TryWithFinallyEnabled: bool with get, set
    abstract TryWithFinallyCollapsedByDefault: bool with get, set
    abstract IfThenElseEnabled: bool with get, set
    abstract IfThenElseCollapsedByDefault: bool with get, set
    abstract CExpressionMembersEnabled: bool with get, set
    abstract CExpressionMembersCollapsedByDefault: bool with get, set
    abstract LoopsEnabled: bool with get, set
    abstract LoopsCollapsedByDefault: bool with get, set
    abstract AttributesEnabled: bool with get, set
    abstract AttributesCollapsedByDefault: bool with get, set
    abstract XmlDocCommentsEnabled: bool with get, set
    abstract XmlDocCommentsCollapsedByDefault: bool with get, set
    abstract CommentsEnabled: bool with get, set
    abstract CommentsCollapsedByDefault: bool with get, set
    abstract TooltipZoomLevel: int with get, set

[<AutoOpen>]
module ServiceProviderUtils =
    type System.IServiceProvider with
        member x.GetService<'T>() = x.GetService(typeof<'T>) :?> 'T
        member x.GetService<'T, 'S>() = x.GetService(typeof<'S>) :?> 'T
