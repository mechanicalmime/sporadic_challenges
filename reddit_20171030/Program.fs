open System

let numberToDay (dayNumber : decimal) = 
    match dayNumber with
    |   0m -> "Saturday"
    |   1m -> "Sunday"
    |   2m -> "Monday"
    |   3m -> "Tuesday"
    |   4m -> "Wednesday"
    |   5m -> "Thursday"
    |   6m -> "Friday"
    |   _ -> "Error"

let zeller (year: decimal) (month: decimal) (day: decimal) = 
    let K = year % 100m
    let J = year / 100m
    Math.Floor((day + Math.Floor(13m*(month + 1m) / 5m) + K + Math.Floor(K / 4m) + Math.Floor(J / 4m) + (5m * J)) % 7m)

[<EntryPoint>]
let main argv =
    let inputs = argv |> Array.map Decimal.Parse
    match inputs with
        | [|year;month;day|] -> zeller year month day |> numberToDay |> Console.WriteLine
        | _ -> Console.WriteLine "Wrong usage"
    0

