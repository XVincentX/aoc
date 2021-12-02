module day2
open FSharpx.Text

type foldState =
    { horizontal: int
      depth: int
      aim: int }

let input =
    "./input/day2input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (Strings.split ' ')
    |> Array.map (fun x -> (x.[0], System.Int32.Parse(x.[1])))

let p1 =
    input
    |> Array.fold
        (fun acc value ->
            match fst value with
            | "forward" -> { acc with horizontal = acc.horizontal + snd value }
            | "down" -> { acc with depth = acc.depth + snd value }
            | "up" -> { acc with depth = acc.depth - snd value }
            | _ -> acc)

        { horizontal = 0; depth = 0; aim = 0 }

printfn "%i %i" p1.depth p1.horizontal

let p2 =
    input
    |> Array.fold
        (fun acc value ->
            match fst value with
            | "forward" ->
                { acc with
                    horizontal = acc.horizontal + snd value
                    depth = acc.depth + snd value * acc.aim }
            | "down" -> { acc with aim = acc.aim + snd value }
            | "up" -> { acc with aim = acc.aim - snd value }
            | _ -> acc)

        { horizontal = 0; depth = 0; aim = 0 }


printfn "%i %i" p2.depth p2.horizontal
