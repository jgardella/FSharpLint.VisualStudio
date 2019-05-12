using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using FSharpLint.VisualStudio;
using FSharpLint.VisualStudio.FSharp.Linting;
using System;
using System.ComponentModel.Composition;

namespace FSharpLint.VisualStudioExtension
{
    [Export(typeof(IQuickInfoSourceProvider))]
    [Name("F# Lint Quick Info Provider")]
    [Order(Before = "Default Quick Info Presenter")]
    [ContentType("F#")]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    internal class LintQuickInfoProvider : IQuickInfoSourceProvider
    {
        private readonly IServiceProvider _serviceProvider;
        private readonly IViewTagAggregatorFactoryService _viewTagAggregatorFactoryService;
     
        [ImportingConstructor]
        public LintQuickInfoProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            IViewTagAggregatorFactoryService viewTagAggregatorFactoryService)
        {
            _serviceProvider = serviceProvider;
            _viewTagAggregatorFactoryService = viewTagAggregatorFactoryService;
        }

        public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer textBuffer)
        {
            return new LintQuickInfoSource(textBuffer, _viewTagAggregatorFactoryService);
        }
    }
}