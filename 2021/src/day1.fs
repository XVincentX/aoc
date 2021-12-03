module day1

type foldState = { inc: int; cur: int }

let input =
    "./input/day1input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map System.Int32.Parse

let p1 =
    input
    |> Array.fold
        (fun acc value ->
            let nextInc =
                match value with
                | x when x > acc.cur -> acc.inc + 1
                | _ -> acc.cur

            { inc = nextInc; cur = value })
        { inc = 0; cur = System.Int32.MaxValue }

printfn "%i" p1.inc

let p2 =
    input
    |> Array.indexed
    |> Array.fold
        (fun acc value ->
            let index = fst value

            let sum =
                input
                |> Array.append [| 0; 0 |]
                |> fun x -> Array.sub x index 3
                |> Array.sum

            let nextInc =
                match sum with
                | x when x > acc.cur -> acc.inc + 1
                | _ -> acc.cur

            { inc = nextInc; cur = sum })
        { inc = 0; cur = System.Int32.MaxValue }

printfn "%i" p2.inc
