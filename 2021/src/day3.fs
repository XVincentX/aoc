module day3

open FSharpx.Text

type foldState = { gamma: string; epsilon: string }

let input =
    "./input/day3input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map Strings.toCharArray

let input_freqs =
    input
    |> Array.transpose
    |> Array.map (fun x -> Array.countBy id x)

let p1 =
    Array.fold
        (fun acc (value: (char * int) []) ->
            let freq1 = value.[0]
            let freq2 = value.[1]

            let most_common =
                match snd freq1 > snd freq2 with
                | true -> fst freq1
                | false -> fst freq2

            let parsed =
                System.Int32.Parse(most_common.ToString())

            let reversed = parsed ^^^ 1

            { gamma = acc.gamma + parsed.ToString()
              epsilon = acc.epsilon + reversed.ToString() })

        { gamma = ""; epsilon = "" }
        input_freqs

printfn "%A" p1
