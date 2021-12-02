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
            if value > acc.cur then
                { inc = acc.inc + 1; cur = value }
            else
                { acc with cur = value })
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
                |> Array.sub <|| (index, 3)
                |> Array.sum

            if sum > acc.cur then
                { inc = acc.inc + 1; cur = sum }
            else
                { inc = acc.inc; cur = sum })
        { inc = 0; cur = System.Int32.MaxValue }

printfn "%i" p2.inc
