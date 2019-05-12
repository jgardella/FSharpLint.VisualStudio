[<AutoOpen>]
module Prelude

    open System
    open System.Diagnostics

    let inline (==) a b = obj.ReferenceEquals(a, b)
    let inline (===) a b = LanguagePrimitives.PhysicalEquality a b
    let inline debug msg = Printf.kprintf Debug.WriteLine msg
    let inline fail msg = Printf.kprintf Debug.Fail msg
    let inline isNull v = match v with | null -> true | _ -> false
    let inline isNotNull v = not (isNull v)
    let inline dispose (disposable:#IDisposable) = disposable.Dispose ()

    let inline Ok a = Choice1Of2 a
    let inline Fail a = Choice2Of2 a
    let inline (|Ok|Fail|) a = a

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Option =
        let inline ofNull value =
            if obj.ReferenceEquals(value, null) then None else Some value

        let inline ofNullable (value: Nullable<'T>) =
            if value.HasValue then Some value.Value else None

        let inline toNullable (value: 'T option) =
            match value with
            | Some x -> Nullable<_> x
            | None -> Nullable<_> ()

        let inline attempt (f: unit -> 'T) = try Some <| f() with _ -> None

        /// Gets the value associated with the option or the supplied default value.
        let inline getOrElse v =
            function
            | Some x -> x
            | None -> v

        /// Gets the option if Some x, otherwise the supplied default value.
        let inline orElse v =
            function
            | Some x -> Some x
            | None -> v

        /// Gets the value if Some x, otherwise try to get another value by calling a function
        let inline getOrTry f =
            function
            | Some x -> x
            | None -> f()

        /// Gets the option if Some x, otherwise try to get another value
        let inline orTry f =
            function
            | Some x -> Some x
            | None -> f()

        /// Some(Some x) -> Some x | None -> None
        let inline flatten x =
            match x with
            | Some x -> x
            | None -> None

        let inline toList x =
            match x with
            | Some x -> [x]
            | None -> []

        let inline iterElse someAction noneAction opt =
            match opt with
            | Some x -> someAction x
            | None   -> noneAction ()
        
    // Async helper functions copied from https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/ControlCollections.Async.fs
    [<RequireQualifiedAccess>]
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Async =
        /// Transforms an Async value using the specified function.
        [<CompiledName("Map")>]
        let map (mapping : 'T -> 'U) (value : Async<'T>) : Async<'U> =
            async {
                // Get the input value.
                let! x = value
                // Apply the mapping function and return the result.
                return mapping x
            }

        // Transforms an Async value using the specified Async function.
        [<CompiledName("Bind")>]
        let bind (binding : 'T -> Async<'U>) (value : Async<'T>) : Async<'U> =
            async {
                // Get the input value.
                let! x = value
                // Apply the binding function and return the result.
                return! binding x
            }

        [<RequireQualifiedAccess>]    
        module Array =
            /// Async implementation of Array.map.
            let map (mapping : 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
                let len = Array.length array
                let result = Array.zeroCreate len

                async { // Apply the mapping function to each array element.
                    for i in 0 .. len - 1 do
                        let! mappedValue = mapping array.[i]
                        result.[i] <- mappedValue

                    // Return the completed results.
                    return result
                }

            /// Async implementation of Array.mapi.
            let mapi (mapping : int -> 'T -> Async<'U>) (array : 'T[]) : Async<'U[]> =
                let len = Array.length array
                let result = Array.zeroCreate len

                async {
                    // Apply the mapping function to each array element.
                    for i in 0 .. len - 1 do
                        let! mappedValue = mapping i array.[i]
                        result.[i] <- mappedValue

                    // Return the completed results.
                    return result
                }

            /// Async implementation of Array.exists.
            let exists (predicate : 'T -> Async<bool>) (array : 'T[]) : Async<bool> =
                let len = Array.length array
                let rec loop i =
                    async {
                        if i >= len then
                            return false
                        else
                            let! found = predicate array.[i]
                            if found then
                                return true
                            else
                                return! loop (i + 1)
                    }
                loop 0

        [<RequireQualifiedAccess>]
        module List =
            let rec private mapImpl (mapping, mapped : 'U list, pending : 'T list) =
                async {
                    match pending with
                    | [] ->
                        // Reverse the list of mapped values before returning it.
                        return List.rev mapped

                    | el :: pending ->
                        // Apply the current list element to the mapping function.
                        let! mappedEl = mapping el

                        // Cons the result to the list of mapped values, then continue
                        // mapping the rest of the pending list elements.
                        return! mapImpl (mapping, mappedEl :: mapped, pending)
                    }

            /// Async implementation of List.map.
            let map (mapping : 'T -> Async<'U>) (list : 'T list) : Async<'U list> =
                mapImpl (mapping, [], list)



    /// Maybe computation expression builder, copied from ExtCore library
    /// https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Control.fs
    [<Sealed>]
    type MaybeBuilder () =
        // 'T -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.Return value: 'T option =
            Some value

        // M<'T> -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.ReturnFrom value: 'T option =
            value

        // unit -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.Zero (): unit option =
            Some ()     // TODO: Should this be None?

        // (unit -> M<'T>) -> M<'T>
        [<DebuggerStepThrough>]
        member __.Delay (f: unit -> 'T option): 'T option =
            f ()

        // M<'T> -> M<'T> -> M<'T>
        // or
        // M<unit> -> M<'T> -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.Combine (r1, r2: 'T option): 'T option =
            match r1 with
            | None ->
                None
            | Some () ->
                r2

        // M<'T> * ('T -> M<'U>) -> M<'U>
        [<DebuggerStepThrough>]
        member inline __.Bind (value, f: 'T -> 'U option): 'U option =
            Option.bind f value

        // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
        [<DebuggerStepThrough>]
        member __.Using (resource: ('T :> System.IDisposable), body: _ -> _ option): _ option =
            try body resource
            finally
                if not <| obj.ReferenceEquals (null, box resource) then
                    resource.Dispose ()

        // (unit -> bool) * M<'T> -> M<'T>
        [<DebuggerStepThrough>]
        member x.While (guard, body: _ option): _ option =
            if guard () then
                // OPTIMIZE: This could be simplified so we don't need to make calls to Bind and While.
                x.Bind (body, (fun () -> x.While (guard, body)))
            else
                x.Zero ()

        // seq<'T> * ('T -> M<'U>) -> M<'U>
        // or
        // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
        [<DebuggerStepThrough>]
        member x.For (sequence: seq<_>, body: 'T -> unit option): _ option =
            // OPTIMIZE: This could be simplified so we don't need to make calls to Using, While, Delay.
            x.Using (sequence.GetEnumerator (), fun enum ->
                x.While (
                    enum.MoveNext,
                    x.Delay (fun () ->
                        body enum.Current)))

    [<Sealed>]
    type AsyncMaybeBuilder () =
        [<DebuggerStepThrough>]
        member __.Return value : Async<'T option> = Some value |> async.Return

        [<DebuggerStepThrough>]
        member __.ReturnFrom value : Async<'T option> = value

        [<DebuggerStepThrough>]
        member __.ReturnFrom (value: 'T option) : Async<'T option> = async.Return value

        [<DebuggerStepThrough>]
        member __.Zero () : Async<unit option> =
            Some () |> async.Return

        [<DebuggerStepThrough>]
        member __.Delay (f : unit -> Async<'T option>) : Async<'T option> = f ()

        [<DebuggerStepThrough>]
        member __.Combine (r1, r2 : Async<'T option>) : Async<'T option> =
            async {
                let! r1' = r1
                match r1' with
                | None -> return None
                | Some () -> return! r2
            }

        [<DebuggerStepThrough>]
        member __.Bind (value: Async<'T option>, f : 'T -> Async<'U option>) : Async<'U option> =
            async {
                let! value' = value
                match value' with
                | None -> return None
                | Some result -> return! f result
            }

        [<DebuggerStepThrough>]
        member __.Bind (value: 'T option, f : 'T -> Async<'U option>) : Async<'U option> =
            async {
                match value with
                | None -> return None
                | Some result -> return! f result
            }

        [<DebuggerStepThrough>]
        member __.Using (resource : ('T :> IDisposable), body : _ -> Async<_ option>) : Async<_ option> =
            try body resource
            finally 
                if isNotNull resource then resource.Dispose ()

        [<DebuggerStepThrough>]
        member x.While (guard, body : Async<_ option>) : Async<_ option> =
            if guard () then
                x.Bind (body, (fun () -> x.While (guard, body)))
            else
                x.Zero ()

        [<DebuggerStepThrough>]
        member x.For (sequence : seq<_>, body : 'T -> Async<unit option>) : Async<_ option> =
            x.Using (sequence.GetEnumerator (), fun enum ->
                x.While (enum.MoveNext, x.Delay (fun () -> body enum.Current)))

        [<DebuggerStepThrough>]
        member inline __.TryWith (computation : Async<'T option>, catchHandler : exn -> Async<'T option>) : Async<'T option> =
                async.TryWith (computation, catchHandler)

        [<DebuggerStepThrough>]
        member inline __.TryFinally (computation : Async<'T option>, compensation : unit -> unit) : Async<'T option> =
                async.TryFinally (computation, compensation)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module AsyncMaybe =
        let inline liftAsync (async : Async<'T>) : Async<_ option> =
            async |> Async.map Some

    [<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Pervasive =
        open System.Threading

    let maybe = MaybeBuilder()
    let asyncMaybe = AsyncMaybeBuilder()
