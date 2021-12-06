module day6

open System
open System.IO
open FSharpx.Text

type foldState = { gamma: string; epsilon: string }


let getCountAt (n: int) (coll: (int * int64) array) =
    match Array.tryFind (fun (num, _) -> num = n) coll with
    | Some (_, c) -> c
    | None -> 0

let setCountAt (n: int) (value: int64) coll =
    match Array.tryFindIndex (fun (num, _) -> num = n) coll with
    | Some x -> Array.updateAt x (n, value) coll
    | None -> Array.append coll [| (n, value) |]

let updateAt (n: int) (fn: int64 -> int64) a =
    let oldV = getCountAt n a
    let newV = fn oldV

    setCountAt n newV a

let input =
    "./input/day6input.txt"
    |> File.ReadAllText
    |> Strings.split ','
    |> Array.countBy id
    |> Array.map (fun (a, b) -> (Int32.Parse a, int64 b))


let nextState (state: (int * int64) array) =
    Array.fold
        (fun s (num, count) ->
            (match num with
             | 0 ->
                 s
                 |> updateAt 6 (fun q -> q + count)
                 |> updateAt 8 (fun q -> q + count)
             | _ -> s |> updateAt (num - 1) (fun q -> q + count))
            |> updateAt num (fun q -> q - count))

        (Array.copy state)
        state

let part1 =
    Array.create 80 0
    |> Array.fold (fun state _ -> nextState state) input
    |> Array.map (fun (_, c) -> c)
    |> Array.sum

printfn "%A" part1

let part2 =
    Array.create 256 0
    |> Array.fold (fun state _ -> nextState state) input
    |> Array.map (fun (_, c) -> c)
    |> Array.sum


printfn "%A" part2
