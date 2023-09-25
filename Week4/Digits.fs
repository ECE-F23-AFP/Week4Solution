module Week4.Digits

open System

let unique s =
    s
    |> string
    |> Seq.toList
    |> Set.ofList
    |> Set.fold (fun s e -> e::s) []
    |> List.map (fun e -> (int (Char.GetNumericValue e))) 