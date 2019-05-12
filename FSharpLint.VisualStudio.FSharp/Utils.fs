[<AutoOpen>]
module FSharpLint.VisualStudio.Utils

open System
open System.Threading
open System.Diagnostics
open FSharpLint.VisualStudio.FSharp

type FileName = string
type FilePath = string

[<Measure>] type FCS

type Point<[<Measure>]'t> = { Line : int; Column : int }
type Range<[<Measure>]'t> = { Start : Point<'t>; End: Point<'t> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Point =
    let make line column : Point<'t> = { Line = line; Column = column }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Range =
    let make startLine startColumn endLine endColumn : Range<'t> =
        { Start = Point.make startLine startColumn
          End = Point.make endLine endColumn }
    
type CurrentLine<[<Measure>]'t> = 
    { Line: string
      File: FileName; Range: Range<'t> }
    member x.EndLine = x.Range.End.Line 

[<NoComparison>]
type PointInDocument<[<Measure>]'t> = 
    { Point: Point<'t>
      Line: string
      Document: string
      File: FileName }
    member x.LineIndex = x.Point.Line
    member x.ColumnIndex = x.Point.Column
    member x.CurrentLine : Lazy<CurrentLine<'t>> = 
        lazy
          { Line = x.Line
            File = x.File
            Range = Range.make x.LineIndex x.ColumnIndex x.LineIndex x.Line.Length }
         
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =
    let toReadOnlyCollection (xs: _ seq) = ResizeArray(xs).AsReadOnly()

[<RequireQualifiedAccess>]
module List =
    /// Fold over the list passing the index and element at that index to a folding function
    let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (xs: 'T list) =
        match xs with 
        | [] -> state
        | _ -> 
            let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
            let rec loop idx s xs = 
                match xs with 
                | [] -> s
                | h::t -> loop (idx+1) (f.Invoke(s,idx,h)) t
            loop 0 state xs

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
    let inline private checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()

    /// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
    /// ~2x faster for floats
    /// ~0.8x slower for ints
    let inline areEqual (xs: 'T []) (ys: 'T []) =
        match xs, ys with
        | null, null -> true
        | [||], [||] -> true
        | null, _ | _, null -> false
        | _ when xs.Length <> ys.Length -> false
        | _ ->
            let mutable break' = false
            let mutable i = 0
            let mutable result = true
            while i < xs.Length && not break' do
                if xs.[i] <> ys.[i] then 
                    break' <- true
                    result <- false
                i <- i + 1
            result

    /// check if subArray is found in the wholeArray starting 
    /// at the provided index
    let inline isSubArray (subArray: 'T []) (wholeArray:'T []) index = 
        if isNull subArray || isNull wholeArray then false
        elif subArray.Length = 0 then true
        elif subArray.Length > wholeArray.Length then false
        elif subArray.Length = wholeArray.Length then areEqual subArray wholeArray else
        let rec loop subidx idx =
            if subidx = subArray.Length then true 
            elif subArray.[subidx] = wholeArray.[idx] then loop (subidx+1) (idx+1) 
            else false
        loop 0 index

    /// Returns true if one array has another as its subset from index 0.
    let startsWith (prefix: _ []) (whole: _ []) =
        isSubArray prefix whole 0

    /// Returns true if one array has trailing elements equal to another's.
    let endsWith (suffix: _ []) (whole: _ []) =
        isSubArray suffix whole (whole.Length-suffix.Length)

    /// Returns a new array with an element replaced with a given value.
    let replace index value (array: _ []) =
        checkNonNull "array" array
        if index >= array.Length then raise (IndexOutOfRangeException "index")
        let res = Array.copy array
        res.[index] <- value
        res

    /// Returns all heads of a given array.
    /// For [|1;2;3|] it returns [|[|1; 2; 3|]; [|1; 2|]; [|1|]|]
    let heads (array: 'T []) =
        checkNonNull "array" array
        let res = Array.zeroCreate<'T[]> array.Length
        for i = array.Length - 1 downto 0 do
            res.[i] <- array.[0..i]
        res

    /// Fold over the array passing the index and element at that index to a folding function
    let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (array: 'T []) =
        checkNonNull "array" array
        if array.Length = 0 then state else
        let folder = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt folder
        let mutable state:'State = state
        let len = array.Length
        for i = 0 to len - 1 do
            state <- folder.Invoke (state, i, array.[i])
        state

    /// pass an array byref to reverse it in place
    let revInPlace (array: 'T []) =
        checkNonNull "array" array
        if areEqual array [||] then () else
        let arrlen, revlen = array.Length-1, array.Length/2 - 1
        for idx in 0 .. revlen do
            let t1 = array.[idx] 
            let t2 = array.[arrlen-idx]
            array.[idx] <- t2
            array.[arrlen-idx] <- t1

    /// Map all elements of the array that satisfy the predicate
    let filterMap predicate mapfn (array: 'T [])  =
        checkNonNull "array" array
        if array.Length = 0 then [||] else
        let result = Array.zeroCreate array.Length
        let mutable count = 0
        for elm in array do
            if predicate elm then 
               result.[count] <- mapfn elm
               count <- count + 1
        if count = 0 then [||] else
        result.[0..count-1]

    /// <summary>
    /// Splits the collection into two (2) collections, containing the elements for which the given function returns
    /// <c>Choice1Of2</c> or <c>Choice2Of2</c>, respectively.
    /// </summary>
    /// <param name="partitioner"></param>
    /// <param name="array"></param>
    /// <returns></returns>
    /// <remarks>
    /// This function is similar to Array.partition, but it allows the returned collections to have different types.
    /// </remarks>
    let mapPartition (partitioner : 'T -> Choice<'U1, 'U2>) array : 'U1[] * 'U2[] =
        // Preconditions
        checkNonNull "array" array
        
        // OPTIMIZATION : If the input array is empty, immediately return empty results.
        if Array.isEmpty array then
            Array.empty, Array.empty
        else
            // Use ResizeArrays to hold the mapped values.
            let resultList1 = ResizeArray ()
            let resultList2 = ResizeArray ()
    
            // Partition the array, adding each element to the ResizeArray
            // specific by the partition function.
            array
            |> Array.iter (fun el ->
                match partitioner el with
                | Choice1Of2 value ->
                    resultList1.Add value
                | Choice2Of2 value ->
                    resultList2.Add value)
    
            // Convert the ResizeArrays to arrays and return them.
            resultList1.ToArray (),
            resultList2.ToArray ()

    let splitByChunks (chunkSizes : int[]) (arr : 'T[]) =
        let rec loop (chunks : int[]) (arr : 'T[]) acc =
            match chunks, arr with
            | [||], _ 
            | _, [||] -> acc
            | _ ->
                let chunk = min chunks.[0] arr.Length
                loop chunks.[1 .. ] arr.[chunk .. ] (arr.[0..(chunk-1)] :: acc)

        loop chunkSizes arr []
        |> Array.ofList
        |> Array.rev

    let toShortHexString (bytes: byte[]) =
        let length = bytes.Length
        let chars = Array.zeroCreate length
        for i in 0..length/2-1 do
            let b1 = byte (bytes.[i] >>> 4)
            chars.[i * 2] <- if b1 > 9uy then char (b1 + 87uy) else char (b1 + 0x30uy)
            let b2 = byte (bytes.[i] &&& 0xFuy)
            chars.[i * 2 + 1] <- if b2 > 9uy then char (b2 + 87uy) else char (b2 + 0x30uy)
        String chars

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Pervasive =
    open System.Threading

// Redirect debug output to F# Interactive for debugging purpose.
// It requires adding '-d:DEBUG' setting in F# Interactive Options.
#if INTERACTIVE
    Debug.Listeners.Add(new TextWriterTraceListener(System.Console.Out)) |> ignore
    Debug.AutoFlush <- true
#endif
   
    let tryCast<'T> (o: obj): 'T option = 
        match o with
        | null -> None
        | :? 'T as a -> Some a
        | _ -> 
            debug "Cannot cast %O to %O" (o.GetType()) typeof<'T>.Name
            None

    /// Load times used to reset type checking properly on script/project load/unload. It just has to be unique for each project load/reload.
    /// Not yet sure if this works for scripts.
    let fakeDateTimeRepresentingTimeLoaded x = DateTime(abs (int64 (match x with null -> 0 | _ -> x.GetHashCode())) % 103231L)
    
    let synchronize f = 
        let ctx = SynchronizationContext.Current
        
        let thread = 
            match ctx with
            | null -> null // saving a thread-local access
            | _ -> Thread.CurrentThread
        f (fun g arg -> 
            let nctx = SynchronizationContext.Current
            match ctx, nctx with
            | null, _ -> g arg
            | _, _ when Object.Equals(ctx, nctx) && thread.Equals(Thread.CurrentThread) -> g arg
            | _ -> ctx.Post((fun _ -> g (arg)), null))

    let memoize f =
        let cache = System.Collections.Generic.Dictionary()
        fun x ->
            match cache.TryGetValue x with
            | true, x -> x
            | _ ->
                let res = f x
                cache.[x] <- res
                res

    type Microsoft.FSharp.Control.Async with
        static member EitherEvent(ev1: IObservable<'T>, ev2: IObservable<'U>) = 
            synchronize (fun f -> 
                Async.FromContinuations((fun (cont, _econt, _ccont) -> 
                    let rec callback1 = 
                        (fun value -> 
                        remover1.Dispose()
                        remover2.Dispose()
                        f cont (Choice1Of2(value)))
                    
                    and callback2 = 
                        (fun value -> 
                        remover1.Dispose()
                        remover2.Dispose()
                        f cont (Choice2Of2(value)))
                    
                    and remover1: IDisposable = ev1.Subscribe(callback1)
                    and remover2: IDisposable = ev2.Subscribe(callback2)
                    ())))

    type Atom<'T when 'T: not struct>(value: 'T) = 
        let refCell = ref value
        
        let rec swap f = 
            let currentValue = !refCell
            let result = Interlocked.CompareExchange<'T>(refCell, f currentValue, currentValue)
            if obj.ReferenceEquals(result, currentValue) then result
            else 
                Thread.SpinWait 20
                swap f
        
        member __.Value = !refCell
        member __.Swap(f: 'T -> 'T) = swap f

    open System.IO

    type Path with
        static member GetFullPathSafe path =
            try Path.GetFullPath path
            with _ -> path

        static member GetFileNameSafe path =
            try Path.GetFileName path
            with _ -> path

    /// Path.Combine
    let (</>) path1 path2 = Path.Combine (path1, path2)

[<RequireQualifiedAccess>]
module Dict = 
    open System.Collections.Generic

    let add key value (dict: #IDictionary<_,_>) =
        dict.[key] <- value
        dict

    let remove (key: 'k) (dict: #IDictionary<'k,_>) =
        dict.Remove key |> ignore
        dict

    let tryFind key (dict: #IDictionary<'k, 'v>) = 
        let mutable value = Unchecked.defaultof<_>
        if dict.TryGetValue (key, &value) then Some value
        else None

    let ofSeq (xs: ('k * 'v) seq) = 
        let dict = Dictionary()
        for k, v in xs do dict.[k] <- v
        dict

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
    let inline toCharArray (str:string) = str.ToCharArray()

    let lowerCaseFirstChar (str: string) =
        if String.IsNullOrEmpty str 
         || Char.IsLower(str, 0) then str else 
        let strArr = toCharArray str
        match Array.tryHead strArr with
        | None -> str
        | Some c  -> 
            strArr.[0] <- Char.ToLower c
            String (strArr)


    let extractTrailingIndex (str: string) =
        match str with
        | null -> null, None
        | _ ->
            let charr = str.ToCharArray() 
            Array.revInPlace charr
            let digits = Array.takeWhile Char.IsDigit charr
            Array.revInPlace digits
            String digits
            |> function
               | "" -> str, None
               | index -> str.Substring (0, str.Length - index.Length), Some (int index)

    /// Remove all trailing and leading whitespace from the string
    /// return null if the string is null
    let trim (value: string) = if isNull value then null else value.Trim()
    
    /// Splits a string into substrings based on the strings in the array separators
    let split options (separator: string []) (value: string) = 
        if isNull value  then null else value.Split(separator, options)

    let (|StartsWith|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.StartsWith pattern then
            Some()
        else None

    let (|Contains|_|) pattern value =
        if String.IsNullOrWhiteSpace value then
            None
        elif value.Contains pattern then
            Some()
        else None
    
    open System.IO

    let getLines (str: string) =
        use reader = new StringReader(str)
        [|
        let line = ref (reader.ReadLine())
        while isNotNull (!line) do
            yield !line
            line := reader.ReadLine()
        if str.EndsWith("\n") then
            // last trailing space not returned
            // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
            yield String.Empty
        |]

    let getNonEmptyLines (str: string) =
        use reader = new StringReader(str)
        [|
        let line = ref (reader.ReadLine())
        while isNotNull (!line) do
            if (!line).Length > 0 then
                yield !line
            line := reader.ReadLine()
        |]

    /// Parse a string to find the first nonempty line
    /// Return null if the string was null or only contained empty lines
    let firstNonEmptyLine (str: string) =
        use reader = new StringReader (str)
        let rec loop (line:string) =
            if isNull line then None 
            elif  line.Length > 0 then Some line
            else loop (reader.ReadLine())
        loop (reader.ReadLine())

open System.Text
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StringBuilder =
    /// Pipelining function for appending a string to a stringbuilder
    let inline append (str:string) (sb:StringBuilder) = sb.Append str

    /// Pipelining function for appending a string with a '\n' to a stringbuilder
    let inline appendLine (str:string) (sb:StringBuilder) = sb.AppendLine str
    
    /// SideEffecting function for appending a string to a stringbuilder
    let inline appendi (str:string) (sb:StringBuilder) = sb.Append str |> ignore

    /// SideEffecting function for appending a string with a '\n' to a stringbuilder
    let inline appendLinei (str:string) (sb:StringBuilder) = sb.AppendLine str |> ignore

module Reflection =
    open System.Reflection
    open Microsoft.FSharp.Reflection

    type private Expr = System.Linq.Expressions.Expression
    let instanceNonPublic = BindingFlags.Instance ||| BindingFlags.NonPublic
    
    let precompileFieldGet<'R>(f : FieldInfo) =
        let p = Expr.Parameter(typeof<obj>)
        let lambda = Expr.Lambda<Func<obj, 'R>>(Expr.Field(Expr.Convert(p, f.DeclaringType) :> Expr, f) :> Expr, p)
        lambda.Compile().Invoke

    // Various flags configurations for Reflection
    let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
    let instanceFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
    let ctorFlags = instanceFlags
    let inline asMethodBase (a : #MethodBase) = a :> MethodBase
    
    let (?) (o : obj) name : 'R = 
        // The return type is a function, which means that we want to invoke a method
        if FSharpType.IsFunction(typeof<'R>) then 
            let argType, _resType = FSharpType.GetFunctionElements(typeof<'R>)
            FSharpValue.MakeFunction(typeof<'R>, 
                fun args -> 
                    // We treat elements of a tuple passed as argument as a list of arguments
                    // When the 'o' object is 'System.Type', we call static methods
                    let methods, instance, args = 
                        let typeInfo = o.GetType()
                        let args = 
                            if argType = typeof<unit> then [||]
                            elif not (FSharpType.IsTuple(argType)) then [| args |]
                            else FSharpValue.GetTupleFields(args)
                        if (typeof<System.Type>).IsAssignableFrom(typeInfo) then 
                            let methods = (unbox<Type> o).GetMethods(staticFlags) |> Array.map asMethodBase
                            let ctors = 
                                (unbox<Type> o).GetConstructors(ctorFlags) 
                                |> Array.map asMethodBase
                            Array.concat [ methods; ctors ], null, args
                        else 
                            typeInfo.GetMethods(instanceFlags) |> Array.map asMethodBase, o, 
                            args
                                         
                    // A simple overload resolution based on the name and number of parameters only
                    let methods = 
                        [ for m in methods do
                            if m.Name = name && m.GetParameters().Length = args.Length then 
                                yield m ]
                                         
                    match methods with
                    | [] -> failwithf "No method '%s' with %d arguments found" name args.Length
                    | _ :: _ :: _ -> 
                        failwithf "Multiple methods '%s' with %d arguments found" name args.Length
                    | [ :? ConstructorInfo as c ] -> c.Invoke(args)
                    | [ m ] -> m.Invoke(instance, args))
            |> unbox<'R>
        else 
            // When the 'o' object is 'System.Type', we access static properties
            let typ, flags, instance = 
                if (typeof<System.Type>).IsAssignableFrom(o.GetType()) then unbox o, staticFlags, null
                else o.GetType(), instanceFlags, o
            
            // Find a property that we can call and get the value
            let prop = typ.GetProperty(name, flags)
            if prop = null then failwithf "Property '%s' not found in '%s' using flags '%A'." name typ.Name flags
            let meth = prop.GetGetMethod(true)
            if prop = null then failwithf "Property '%s' found, but doesn't have 'get' method." name
            meth.Invoke(instance, [||]) |> unbox<'R>

    let (?<-) (o: obj) name value =
      o.GetType().GetProperty(name).SetValue(o, value, null)

module File =
    open System.IO

    let tryGetLastWriteTime file = Option.attempt (fun _ -> FileInfo(file).LastWriteTimeUtc)

type Profiler() =
    let measures = ResizeArray()
    let total = Stopwatch.StartNew()

    member __.Time msg f = 
        let sw = Stopwatch.StartNew()
        let res = f()
        measures.Add(msg, sw.Elapsed)
        res

    member __.TimeAsync msg f = async {
        let sw = Stopwatch.StartNew()
        let! res = f()
        measures.Add(msg, sw.Elapsed)
        return res }

    member __.Stop() = total.Stop()
    
    member __.Result =
        sprintf
            "\nTotal = %O\n%s" 
            total.Elapsed
            (measures 
             |> Seq.groupBy (fun (msg, _) -> msg)
             |> Seq.map (fun (msg, ts) -> 
                 msg, TimeSpan.FromTicks (ts |> Seq.sumBy (fun (_, t) -> t.Ticks)))
             |> Seq.sortBy (fun (_, t) -> -t)
             |> Seq.fold (fun (acc: StringBuilder) (msg, t) -> 
                 acc.AppendLine (sprintf "%s, %O" msg t)) (StringBuilder())
             |> string)

    member __.Elapsed = total.Elapsed        

type String with
    /// Splits a string into lines for all platform's linebreaks.
    /// If the string mixes windows, mac, and linux linebreaks, all will be respected
    member self.ToLineArray () = String.getLines self

    /// Return substring starting at index, returns the same string if given a negative index
    /// if given an index > string.Length returns empty string
    member self.SubstringSafe index =
        if index < 0 then self elif index > self.Length then "" else self.Substring index

open System.Diagnostics
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open FSharp.Compiler
//open Microsoft.FSharp.Compiler.Range

let fromRange (snapshot: ITextSnapshot) (startLine, startColumn, endLine, endColumn) =
    Debug.Assert(startLine <= endLine, sprintf "startLine = %d, endLine = %d" startLine endLine)
    Debug.Assert(startLine <> endLine || startColumn <= endColumn, 
                 sprintf "Single-line pos, but startCol = %d, endCol = %d" startColumn endColumn)
    try 
        let startPos = snapshot.GetLineFromLineNumber(startLine - 1).Start.Position + startColumn
        let endPos = snapshot.GetLineFromLineNumber(endLine - 1).Start.Position + endColumn
        Debug.Assert(startPos <= endPos, sprintf "startPos = %d, endPos = %d" startPos endPos)
        let length = endPos - startPos
        Some (SnapshotSpan(snapshot, startPos, length))
    with e ->
        fail "Attempting to create a SnapshotSpan (StartLine = %d, StartColumn = %d, EndLine = %d, EndColumn = %d) in a snapshot length = %d results in: %O"
             startLine startColumn endLine endColumn snapshot.Length e
        //Logging.logException e
        None
    
/// Retrieve snapshot span from VS zero-based positions
let fromFSharpRange (snapshot: ITextSnapshot) (r: Range.range) = 
    fromRange snapshot (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

let [<Literal>] FSharpProjectKind = "{F2A71F9B-5D33-465A-A702-920D77279786}"

let isFSharpProject (project: EnvDTE.Project) = 
    isNotNull project && isNotNull project.Kind && project.Kind.Equals(FSharpProjectKind, StringComparison.OrdinalIgnoreCase)

let isPhysicalFolderKind (kind: string) =
    kind.Equals(EnvDTE.Constants.vsProjectItemKindPhysicalFolder, StringComparison.OrdinalIgnoreCase)

let isPhysicalFileKind (kind: string) =
    kind.Equals(EnvDTE.Constants.vsProjectItemKindPhysicalFile, StringComparison.OrdinalIgnoreCase)

let isPhysicalFileOrFolderKind kind =
    isNotNull kind && (isPhysicalFolderKind kind) || isPhysicalFileKind kind

let isPhysicalFolder (item: EnvDTE.ProjectItem) =
    isNotNull item && isNotNull item.Kind && isPhysicalFolderKind item.Kind

let isPhysicalFile (item: EnvDTE.ProjectItem) =
    isNotNull item && isNotNull item.Kind && isPhysicalFileKind item.Kind

let isPhysicalFileOrFolder (item: EnvDTE.ProjectItem) =
    isNotNull item && isPhysicalFileOrFolderKind item.Kind

let filePath (item: EnvDTE.ProjectItem) =
    Debug.Assert(item.FileCount = 1s, "Item should be unique.")
    item.FileNames(1s) //1 based indexing

//let inline private isTypeParameter (prefix: char) (s: string) =
//    match s.Length with
//    | 0 | 1 -> false
//    | _ -> s.[0] = prefix && IdentifierUtils.isIdentifier s.[1..]

//let isGenericTypeParameter = isTypeParameter '''
//let isStaticallyResolvedTypeParameter = isTypeParameter '^'

type SnapshotPoint with
    member inline x.Line = x.Snapshot.GetLineNumberFromPosition x.Position
    member inline x.Column = x.Position - x.GetContainingLine().Start.Position
    member inline x.LineText = x.GetContainingLine().GetText()
    member x.InSpan (span: SnapshotSpan) = 
        // The old snapshot might not be available anymore, we compare on updated snapshot
        let point = x.TranslateTo(span.Snapshot, PointTrackingMode.Positive)
        point.CompareTo span.Start >= 0 && point.CompareTo span.End <= 0
    member x.ToPoint =
        x.Line, x.Column
    
    member x.MakePointInDocument filename source : PointInDocument<FCS> =
        let line = x.Line
        let col = x.Column
        let lineStr = x.LineText
        { Point = Point.make line col; Line = lineStr; File = filename; Document = source }

type ITextSnapshot with
    /// SnapshotSpan of the entirety of this TextSnapshot
    member x.FullSpan =
        SnapshotSpan(x, 0, x.Length)

    /// Get the start and end line numbers of a snapshotSpan based on this textSnapshot
    /// returns a tuple of (startLineNumber, endLineNumber)
    member inline x.LineBounds (snapshotSpan:SnapshotSpan) =
        let startLineNumber = x.GetLineNumberFromPosition (snapshotSpan.Span.Start)
        let endLineNumber = x.GetLineNumberFromPosition (snapshotSpan.Span.End)
        (startLineNumber, endLineNumber)

    /// Get the text at line `num`
    member inline x.LineText num =  x.GetLineFromLineNumber(num).GetText()

type SnapshotSpan with
    member inline x.StartLine = x.Snapshot.GetLineFromPosition (x.Start.Position)
    member inline x.StartLineNum = x.Snapshot.GetLineNumberFromPosition x.Start.Position
    member inline x.StartColumn = x.Start.Position - x.StartLine.Start.Position 
    member inline x.EndLine = x.Snapshot.GetLineFromPosition (x.End.Position)
    member inline x.EndLineNum  = x.Snapshot.GetLineNumberFromPosition x.End.Position
    member inline x.EndColumn = x.End.Position - x.EndLine.Start.Position

    //member x.ModStart num = VS.Snapshot.mkSpan (x.Start + num) x.End 
    //member x.ModEnd num = VS.Snapshot.mkSpan x.Start (x.End + num)
    //member x.ModBoth m1 m2 = VS.Snapshot.mkSpan (x.Start + m1) (x.End + m2)

    /// get the position of the token found at (line,.col) if token was not found then -1,-1
    member x.PositionOf (token:string) =
        let firstLine = x.StartLineNum
        let lastLine = x.EndLineNum
        let lines = [| for idx in firstLine .. lastLine -> x.Snapshot.LineText idx |]

        let withinBounds (line, col) =
            match line, col with
            | -1,-1 -> -1,-1 // fast terminate if token wasn't found
            |  l, c when c < x.StartColumn &&  l = firstLine -> -1,-1
            |  l, c when c > x.EndColumn &&  l = lastLine -> -1,-1
            | _ -> line,col

        let rec loop idx =
            if idx > lines.Length then -1,-1 else
            match lines.[idx].IndexOf(token) with
            | -1 -> loop (idx+1)
            | toki -> (firstLine+idx,toki)
        
        loop 0 |> withinBounds

    /// Return corresponding zero-based FCS range
    /// (lineStart, colStart, lineEnd, colEnd)
    member inline x.ToRange () : Range<FCS> =
        Range.make x.StartLineNum x.StartColumn x.EndLineNum (x.EndColumn - 1)

    member inline x.MakeCurrentLine (filename) =
        { Line = x.Start.GetContainingLine().GetText(); Range = x.ToRange(); File = filename }
    
    static member MakeFromRange (snapshot: ITextSnapshot) (range:Range<FCS>) =
        let startPos = snapshot.GetLineFromLineNumber(range.Start.Line).Start.Position + range.Start.Column
        let endPos = snapshot.GetLineFromLineNumber(range.End.Line).Start.Position + range.End.Column
        SnapshotSpan(snapshot, startPos, endPos - startPos)

type ITextBuffer with
    member x.GetSnapshotPoint (position: CaretPosition) = 
        Option.ofNullable <| position.Point.GetPoint(x, position.Affinity)

    member x.TriggerTagsChanged (sender: obj) (event: Event<_,_>) =
        let span = x.CurrentSnapshot.FullSpan
        event.Trigger(sender, SnapshotSpanEventArgs(span))

type ITextView with
    /// Return a simple zero-based (line, column) tuple from 
    /// actual caret position in this ITextView
    member x.GetCaretPosition () =
        maybe {
          let! point = x.TextBuffer.GetSnapshotPoint x.Caret.Position
          let line = point.Snapshot.GetLineNumberFromPosition point.Position
          let col = point.Position - point.GetContainingLine().Start.Position
          return (line, col)
        }

    ///// Return Microsoft.FSharp.Compiler.Range.pos from actual 
    ///// caret position in this ITextView taking care of off by 
    ///// 1 indexing between VS and FCS
    //member x.PosAtCaretPosition () =
    //    maybe {
    //      let! line, col = x.GetCaretPosition()
    //      return Microsoft.FSharp.Compiler.Range.mkPos (line + 1) (col + 1)
    //    }
    
    member x.SnapshotPointAtCaret = x.TextBuffer.GetSnapshotPoint x.Caret.Position

open System.Runtime.InteropServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.ComponentModelHost

// This is for updating documents after refactoring
// Reference at https://pytools.codeplex.com/SourceControl/latest#Python/Product/PythonTools/PythonToolsPackage.cs

//type DocumentUpdater(serviceProvider: IServiceProvider) = 
//    member __.OpenDocument(fileName: string, [<Out>] viewAdapter: byref<IVsTextView>, pWindowFrame: byref<IVsWindowFrame>) = 
//        let hierarchy = ref null
//        let itemId = ref 0u
//        VsShellUtilities.OpenDocument(serviceProvider, fileName, Guid.Empty, hierarchy, itemId, &pWindowFrame, &viewAdapter)

//    member x.GetBufferForDocument(fileName: string) = 
//        let viewAdapter = ref null
//        let frame = ref null
//        x.OpenDocument(fileName, viewAdapter, frame)

//        let lines = ref null
//        ErrorHandler.ThrowOnFailure((!viewAdapter).GetBuffer(lines)) |> ignore

//        let componentModel = serviceProvider.GetService<IComponentModel, SComponentModel>()
//        let adapter = componentModel.GetService<IVsEditorAdaptersFactoryService>()
//        adapter.GetDocumentBuffer(!lines)

//    member __.BeginGlobalUndo(key: string) = 
//        let linkedUndo = serviceProvider.GetService<IVsLinkedUndoTransactionManager, SVsLinkedUndoTransactionManager>()
//        ErrorHandler.ThrowOnFailure(linkedUndo.OpenLinkedUndo(uint32 LinkedTransactionFlags2.mdtGlobal, key)) |> ignore
//        linkedUndo

//    member __.EndGlobalUndo(linkedUndo: IVsLinkedUndoTransactionManager) = 
//        ErrorHandler.ThrowOnFailure(linkedUndo.CloseLinkedUndo()) |> ignore

/// Fix invalid symbols if they appear to have redundant suffix and prefix. 
/// All symbol uses are assumed to belong to a single snapshot.
let fixInvalidSymbolSpans (snapshot: ITextSnapshot) (lastIdent: string) spans =
    spans
    |> Seq.choose (fun (isDef, span: SnapshotSpan) -> 
        let newLastIdent = span.GetText()
        let index = newLastIdent.LastIndexOf(lastIdent, StringComparison.Ordinal)
        if index > 0 then 
            // Sometimes FCS returns a composite identifier for a short symbol, so we truncate the prefix
            // Example: newLastIdent --> "x.Length", lastIdent --> "Length"
            Some (isDef, SnapshotSpan(snapshot, span.Start.Position + index, span.Length - index))
        elif index = 0 && newLastIdent.Length > lastIdent.Length then
            // The returned symbol use is too long; we truncate its redundant suffix
            // Example: newLastIdent --> "Length<'T>", lastIdent --> "Length"
            Some (isDef, SnapshotSpan(snapshot, span.Start.Position, lastIdent.Length))
        elif index = 0 then
            Some (isDef, span)
        else
            // In the case of attributes, a returned symbol use may be a part of original text
            // Example: newLastIdent --> "Sample", lastIdent --> "SampleAttribute"
            let index = lastIdent.LastIndexOf(newLastIdent, StringComparison.Ordinal)
            if index >= 0 then
                Some (isDef, span)
            else None)
    |> Seq.distinctBy (fun (_, span) -> span.Start.Position)
    |> Seq.toList

open EnvDTE
open VSLangProj

let tryGetProject (hierarchy: IVsHierarchy) =
    if isNull hierarchy then
        None
    else
        match hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, int __VSHPROPID.VSHPROPID_ExtObject) with
        | VSConstants.S_OK, p ->
            tryCast<Project> p
        | _ -> 
            None

let tryFindProject (rdt: IVsRunningDocumentTable) fileName =
    match rdt.FindAndLockDocument(uint32 _VSRDTFLAGS.RDT_NoLock, fileName) with
    | VSConstants.S_OK, hier, _, _, _ ->
        tryGetProject hier            
    | _ -> None

type DTE with
    member x.GetActiveDocument() =
        let doc =
            maybe {
                let! doc = Option.attempt (fun _ -> x.ActiveDocument) |> Option.bind Option.ofNull
                let! _ = Option.ofNull doc.ProjectItem
                return doc }
        match doc with
        | None -> debug "There is no active document or its project item is null."
        | _ -> ()
        doc

    member x.TryGetProperty(category, page, name) = 
        x.Properties(category, page)
        |> Seq.cast
        |> Seq.tryPick (fun (prop: Property) ->
            if prop.Name = name then Some (prop.Value)
            else None)

type ProjectItem with
    member x.VSProject =
        Option.ofNull x
        |> Option.bind (fun item ->
            Option.attempt (fun _ -> item.ContainingProject.Object :?> VSProject)
            |> Option.bind Option.ofNull)

    member x.TryGetProperty name = 
        let property = x.Properties |> Seq.cast<Property> |> Seq.tryFind (fun p -> p.Name = name)
        match property with
        | Some p -> Some (p.Value :?> string)
        | None -> None

    member x.GetProperty name = 
        let property = x.TryGetProperty name
        match property with
        | Some p -> p
        | None -> raise(new ArgumentException("name"))

type Project with
    member x.GetReferencedProjects() = 
        (x.Object :?> VSProject).References
        |> Seq.cast<Reference>
        |> Seq.choose (fun reference ->
            maybe {
                let! reference = Option.ofNull reference
                let! project = Option.attempt (fun _ -> reference.SourceProject)
                return! Option.ofNull project
            })
        |> Seq.toList

    member x.GetReferencedFSharpProjects() = x.GetReferencedProjects() |> List.filter isFSharpProject

    member x.VSProject =
        Option.ofNull x
        |> Option.bind (fun project ->
            Option.attempt (fun _ -> project.Object :?> VSProject)
            |> Option.bind Option.ofNull)

let inline ensureSucceeded hr = 
    ErrorHandler.ThrowOnFailure hr
    |> ignore

let getSelectedFromSolutionExplorer<'T> (dte: EnvDTE80.DTE2) =
    Option.attempt (fun _ -> dte.ToolWindows.SolutionExplorer)
    |> function Some x -> x.SelectedItems :?> UIHierarchyItem[] | None -> [||]
    |> Seq.choose (fun x ->
         match x.Object with
         | :? 'T as p -> Some p
         | _ -> None)
    |> Seq.toList

open System.Windows
open System.Windows.Interop

/// Display a modal dialog and set VS window as the owner
let showDialog (wnd: Window) (shell: IVsUIShell) = 
    match shell.GetDialogOwnerHwnd() with
    | VSConstants.S_OK, hwnd ->
        let helper = WindowInteropHelper(wnd)
        helper.Owner <- hwnd
        wnd.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        try
            if ErrorHandler.Failed(shell.EnableModeless(0)) then Some false
            else wnd.ShowDialog() |> Option.ofNullable
        finally
            shell.EnableModeless(1) |> ignore
    | _ -> 
        None

open System.Threading
open System.Windows.Input

[<Literal>]
let private UnassignedThreadId = -1

type ForegroundThreadGuard private() = 
    static let mutable threadId = UnassignedThreadId

    static member BindThread() =
        if threadId <> UnassignedThreadId then 
            fail "Thread is already set"
        threadId <- Thread.CurrentThread.ManagedThreadId

    static member CheckThread() =
        if threadId = UnassignedThreadId then 
            fail "Thread not set"
        if threadId <> Thread.CurrentThread.ManagedThreadId then
            fail "Accessed from the wrong thread"

type Async with
    /// An equivalence of Async.StartImmediate which catches and logs raised exceptions
    static member StartImmediateSafe(computation, ?cancellationToken) =
        let comp =
            async {
                try
                    return! computation
                with e ->
                    fail "The following exception occurs inside async blocks: %O" e
                    Logging.logException e
            }
        Async.StartImmediate(comp, ?cancellationToken = cancellationToken)

    /// An equivalence of Async.Start which catches and logs raised exceptions
    static member StartInThreadPoolSafe(computation, ?cancellationToken) =
        let comp =
            async {
                try
                    return! computation
                with e ->
                    fail "The following exception occurs inside async blocks: %O" e
                    Logging.logException e
            }
        Async.Start(comp, ?cancellationToken = cancellationToken)

/// Provides an IDisposable handle which allows us to override the cursor cleanly as well as restore whenever needed
type CursorOverrideHandle(newCursor) =
    let mutable disposed = false

    let originalCursor = Mouse.OverrideCursor
    let restore () = 
        if not disposed then
            Mouse.OverrideCursor <- originalCursor
            disposed <- true

    do Mouse.OverrideCursor <- newCursor

    member __.Restore() = restore()

    interface IDisposable with 
        member __.Dispose() = restore()

module internal Cursor =
    let wait() = new CursorOverrideHandle(System.Windows.Input.Cursors.Wait)

/// Try to run a given function, resorting to a default value if it throws exceptions
let protectOrDefault f defaultVal =
    try
        f()
    with e ->
        Logging.logException e
        defaultVal

/// Try to run a given async computation, catch and log its exceptions
let protectAsync a =
    async {
        let! res = Async.Catch a
        return 
            match res with 
            | Choice1Of2 () -> ()
            | Choice2Of2 e ->
                Logging.logException e
                ()
    }

/// Try to run a given function and catch its exceptions
let protect f = protectOrDefault f ()

/// Execute a function and record execution time
let time label f =
    let sw = Stopwatch.StartNew()
    let result = f()
    sw.Stop()
    debug "%s took: %i ms" label sw.ElapsedMilliseconds
    result

type IServiceProvider with
    /// Go to exact location in a given file.
    member serviceProvider.NavigateTo (fileName, startRow, startCol, endRow, endCol) =
        let mutable hierarchy = Unchecked.defaultof<_>
        let mutable itemId = Unchecked.defaultof<_>
        let mutable windowFrame = Unchecked.defaultof<_>
        let isOpened = 
            VsShellUtilities.IsDocumentOpen(
                serviceProvider, 
                fileName, 
                Constants.guidLogicalTextView,
                &hierarchy,
                &itemId,
                &windowFrame)
        let canShow = 
            if isOpened then true
            else
                // TODO: track the project that contains document and open document in project context
                try
                    VsShellUtilities.OpenDocument(
                        serviceProvider, 
                        fileName, 
                        Constants.guidLogicalTextView, 
                        &hierarchy,
                        &itemId,
                        &windowFrame)
                    true
                with _ -> false
        if canShow then
            windowFrame.Show()
            |> ensureSucceeded

            let vsTextView = VsShellUtilities.GetTextView windowFrame
            let vsTextManager = serviceProvider.GetService<IVsTextManager, SVsTextManager>()
            let mutable vsTextBuffer = Unchecked.defaultof<_>
            vsTextView.GetBuffer (&vsTextBuffer)
            |> ensureSucceeded

            vsTextManager.NavigateToLineAndColumn (vsTextBuffer, ref Constants.guidLogicalTextView, startRow, startCol, endRow, endCol)
            |> ensureSucceeded

    /// Get the IWPFTextView of a document if it is open
    member serviceProvider.GetWPFTextViewOfDocument fileName =
        let mutable hierarchy = Unchecked.defaultof<_>
        let mutable itemId = Unchecked.defaultof<_>
        let mutable windowFrame = Unchecked.defaultof<_>
        if VsShellUtilities.IsDocumentOpen
               (serviceProvider, fileName, Constants.guidLogicalTextView, &hierarchy, &itemId, &windowFrame) then
            let vsTextView = VsShellUtilities.GetTextView windowFrame 
            let componentModel = serviceProvider.GetService<IComponentModel, SComponentModel>()
            let vsEditorAdapterFactoryService =  componentModel.GetService<IVsEditorAdaptersFactoryService>()
            Some (vsEditorAdapterFactoryService.GetWpfTextView vsTextView)
        else None

    member serviceProvider.GetDte() = serviceProvider.GetService<EnvDTE.DTE, SDTE>()

open System.IO

let isSourceFile path =
    let ext = Path.GetExtension path
    String.Equals (ext, ".fsx", StringComparison.OrdinalIgnoreCase) 
    || String.Equals (ext, ".fsscript", StringComparison.OrdinalIgnoreCase) 
    || String.Equals (ext, ".fs", StringComparison.OrdinalIgnoreCase)

let isSignatureFile path =
    let ext = Path.GetExtension path
    String.Equals(ext, ".fsi", StringComparison.OrdinalIgnoreCase)

let listFSharpProjectsInSolution (dte: DTE) =
    let rec handleProject (p: Project) = 
        if p === null then []
        elif isFSharpProject p then [ p ]
        elif p.Kind = EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder then 
            handleProjectItems p.ProjectItems
        else []
        
    and handleProjectItems (items: ProjectItems) =
        [ for pi in items do
            yield! handleProject pi.SubProject ]

    [ for p in dte.Solution.Projects do
        yield! handleProject p ]

[<NoComparison; NoEquality>]
type CallInUIContext = 
    | CallInUIContext of ((unit -> unit) -> Async<unit>)
    static member FromCurrentThread() = 
        let uiContext = SynchronizationContext.Current
        CallInUIContext (fun f ->
            async {
                let ctx = SynchronizationContext.Current
                do! Async.SwitchToContext uiContext
                protect f
                do! Async.SwitchToContext ctx
            })