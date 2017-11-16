open System

let removeOverlapping date dates =
    match date with
    | (start, ends) -> List.filter (fun (s, e) -> start > e || ends < s) dates

let rec optimalDates dates =
    match dates with
    | [] -> []
    | d :: ds -> 
        let optimalIncludingElement = (ds |> removeOverlapping d |> optimalDates)
        let optimalExcludingElement = ds |> optimalDates
        if 1 + optimalIncludingElement.Length > optimalExcludingElement.Length then
            d :: optimalIncludingElement
        else
            optimalExcludingElement

let compareTuples t1 t2 : int =
    let (x1, _) = t1
    let (x2, _) = t2
    x1 - x2

[<EntryPoint>]
let main argv =
    let requestNumber = Console.ReadLine() |> int
    let startingDates = Console.ReadLine().Split [|' '|] |> Array.map int
    let endingDates = Console.ReadLine().Split [|' '|] |> Array.map int
    let dates = Array.zip startingDates endingDates |> Array.toList |> List.sortWith compareTuples

    Console.WriteLine(sprintf "%A" (optimalDates dates))
    0 