using System;
using System.ComponentModel.Design;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using EnvDTE;
using EnvDTE80;
using System.ComponentModel.Composition;
using System.Diagnostics;
using Microsoft.VisualStudio.ComponentModelHost;
using FSharpLint.VisualStudio;
using FSharpLint.VisualStudio.FSharp;
using static FSharpLint.VisualStudio.Utils;

namespace FSharpLint.VisualStudioExtension
{
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [ProvideMenuResource(resourceID: "Menus.ctmenu", version: 1)]
    [InstalledProductRegistration("#110", "#112", "1.0", IconResourceID = 400)]
    [ProvideBindingPath]
    [Guid(VSPackage.PackageGuidString)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.SolutionExists_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.FSharpProject_string)]
    [ProvideAutoLoad(VSConstants.UICONTEXT.NoSolution_string)]
    public class VSPackage : Package
    {
        public const string PackageGuidString = "651e3218-d95b-4ad0-a33f-5b181499a33a";
        private uint pctCookie;
        private uint objectManagerCookie;

        internal static Lazy<DTE2> DTE
            = new Lazy<DTE2>(() => ServiceProvider.GlobalProvider.GetService(typeof(DTE)) as DTE2);

        protected override void Initialize()
        {
            base.Initialize();
            ForegroundThreadGuard.BindThread();
        }

    }
}