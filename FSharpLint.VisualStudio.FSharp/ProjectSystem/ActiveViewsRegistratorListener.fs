namespace FSharp.Editing.VisualStudio.Navigation

open System.ComponentModel.Composition
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open FSharpLint.VisualStudio.FSharp.ProjectSystem

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<ContentType("F#")>]
[<TextViewRole(PredefinedTextViewRoles.Interactive)>]
type ActiveViewRegistratorListener [<ImportingConstructor>]([<Import>] openDocumentsTracker: IVSOpenDocumentsTracker) = 
    interface IWpfTextViewCreationListener with
        member __.TextViewCreated view = openDocumentsTracker.RegisterView view