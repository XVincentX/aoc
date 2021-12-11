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
    "./input/day9input.txt"
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

let lowPoints =
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

let part1 =
    Array.sum lowPoints + Array.length lowPoints

printfn "%i" part1

let rec basinLen x y curValue visited =
    if Set.contains (x, y) visited || curValue = 9 then
        visited
    else
        let mutable nextSet = (Set.add (x, y) visited)

        let a = getOrMax input x (y + 1)
        let b = getOrMax input x (y - 1)

        let c = getOrMax input (x + 1) y
        let d = getOrMax input (x - 1) y

        if System.Math.Abs(curValue - c) = 1 then
            nextSet <- basinLen (x + 1) y c nextSet

        if System.Math.Abs(curValue - d) = 1 then
            nextSet <- basinLen (x - 1) y d nextSet

        if System.Math.Abs(curValue - a) = 1 then
            nextSet <- basinLen x (y + 1) a nextSet

        if System.Math.Abs(curValue - b) = 1 then
            nextSet <- basinLen x (y - 1) b nextSet

        nextSet

let basins =
    foldi
        (fun x y state value ->
            let a = getOrMax input (x + 1) y
            let b = getOrMax input (x - 1) y
            let c = getOrMax input x (y + 1)
            let d = getOrMax input x (y - 1)

            let take =
                value < a && value < b && value < c && value < d

            if take then
                Array.append state [| basinLen x y value Set.empty |]
            else
                state)
        Array.empty
        input

let part2 =
    basins
    |> Array.map Set.count
    |> Array.sort
    |> Array.rev
    |> Array.take 3
    |> Array.reduce (fun x y -> x * y)

printfn "%A" part2
