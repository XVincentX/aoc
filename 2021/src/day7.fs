module day7

open System.IO
open FSharpx.Text

let input =
    "./input/day7input.txt"
    |> File.ReadAllText
    |> Strings.split ','
    |> Array.map System.Int32.Parse

let distance x = [| for i in 0 .. x -> i |] |> Array.sum

let solution (fn: int -> int) =
    Array.fold
        (fun state targetPos ->
            let distanceForPos =
                Array.fold
                    (fun st elementPos ->
                        let cost =
                            ((targetPos - elementPos): int)
                            |> System.Math.Abs
                            |> fn

                        Array.append st [| cost |])
                    Array.empty<int>
                    input

            Array.append state distanceForPos)
        Array.empty<int>
        [| for i in 0 .. Array.max input -> i |]
    |> Array.chunkBySize input.Length
    |> Array.map Array.sum
    |> Array.min


printfn "%i" (solution id)
printfn "%i" (solution distance)
