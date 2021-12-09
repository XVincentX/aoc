module day9

open System.IO
open FSharpx.Text

let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T [,]) =
    seq {
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                yield (x, y, array.[x, y])
    }
    |> Seq.fold (fun acc (x, y, e) -> folder x y acc e) state

let input =
    "/Users/vincenzoc/dev/aoc/2021/input/day9input.txt"
    |> File.ReadAllLines
    |> Array.map Strings.toCharArray
    |> array2D
    |> Array2D.map (fun t -> System.Int32.Parse(t.ToString()))

let get defaultValue array x y =
    try
        Array2D.get array x y
    with
    | _ -> defaultValue

let getOrMax = get System.Int32.MaxValue

let res =
    foldi
        (fun x y state value ->
            let a = getOrMax input (x + 1) y
            let b = getOrMax input (x - 1) y
            let c = getOrMax input x (y + 1)
            let d = getOrMax input x (y - 1)

            let take =
                value < a && value < b && value < c && value < d

            if take then
                Array.append state [| value |]
            else
                state)
        Array.empty<int>
        input

let part1 = Array.sum res + Array.length res
printfn "%i" part1
