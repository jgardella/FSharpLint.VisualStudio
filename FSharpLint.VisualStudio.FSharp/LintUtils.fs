namespace FSharpLint.VisualStudio.FSharp.Linting

open System
open System.IO
open FSharpLint.Framework.Configuration
open FSharpLint.VisualStudio.FSharp
open FSharpLint.Application

module LintUtils =
    open FSharpLint.VisualStudio.Utils

    let getProjectPaths (dte: EnvDTE.DTE) =
        listFSharpProjectsInSolution dte
        |> List.choose (fun project -> 
                let projectFilePath = project.FullName
                if not (String.IsNullOrEmpty projectFilePath) then
                    let projectDirectoryPath = Path.GetDirectoryName projectFilePath
                    Some projectDirectoryPath
                else
                    None)

    let getSolutionPath (dte: EnvDTE.DTE) =
        let solutionFilePath = dte.Solution.FullName
        if not (String.IsNullOrEmpty solutionFilePath) then 
            let solutionDirectoryPath = Path.GetDirectoryName solutionFilePath
            Some solutionDirectoryPath
        else
            None

    let tryLoadConfig path = 
        let filename = 
            path 
            |> String.concat (Path.DirectorySeparatorChar.ToString())
            |> fun x -> x + "/" + SettingsFileName

        if File.Exists(filename) then
            try
                File.ReadAllText filename |> ConfigurationManagement.loadConfigurationFile |> Some
            with
                | ConfigurationManager.ConfigurationException(message) ->
                    Logging.logWarning (fun _ -> sprintf "Failed to load config file %s: %s" filename message)
                    None
                | e ->
                    Logging.logWarning (fun _ -> sprintf "Failed to load config file %s: %s" filename e.Message)
                    None
        else
            None

    let private directorySeparator = Path.DirectorySeparatorChar.ToString()

    let rec private listStartsWith = function
        | (_, []) -> true
        | (x::list, y::startsWithList) when x = y ->
            listStartsWith (list, startsWithList)
        | _ -> false
