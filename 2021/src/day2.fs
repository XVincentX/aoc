module day2

type foldState = { forward: int; depth: int; aim: int }

let input =
    "./input/day2input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (fun x -> x.Split " ")
    |> Array.map(fun x -> (x[0], System.Int32.Parse(x[1])))

let p1 =
    input
    |> Array.fold
        (fun acc value ->
            match fst value with
            | "forward" -> { acc with forward = acc.forward + snd value }
            | "down" -> { acc with depth = acc.depth + snd value }
            | "up" -> { acc with depth = acc.depth - snd value }
            | _ -> acc)

        { forward = 0; depth = 0; aim = 0 }

printfn "%i %i" p1.depth p1.forward

let p2 =
    input
    |> Array.fold
        (fun acc value ->
            match fst value with
            | "forward" ->
                { acc with
                    forward = acc.forward + snd value
                    depth = snd value * acc.aim }
            | "down" -> { acc with depth = acc.depth + snd value }
            | "up" -> { acc with depth = acc.depth - snd value }
            | _ -> acc)

        { forward = 0; depth = 0; aim = 0 }


printfn "%i %i" p2.depth p2.forward
