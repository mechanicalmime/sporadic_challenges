open System

let rec cannibals query elements = 
    match elements with
    | [] -> 0
    | h::tail when h >= query -> 1 + (cannibals query tail)
    | h::tail when h < query && (query - h) <= tail.Length -> 1 + (cannibals query tail.[0..(tail.Length - (query - h) - 1)])
    | h::tail when h < query && (query - h) > tail.Length -> 0
    | _ -> 0

let rec executeCannibals (queries, elements) = 
    match queries with
    | [] -> []
    | query::tail ->  (cannibals query elements)::(executeCannibals (tail, elements))

let rec readFromConsole n = 
    match n with
    | n when n <= 0 -> Console.ReadLine()
    | n  -> string(char (Console.Read())) + readFromConsole (n - 1)

let parseInput elements queries = 
    let readElements = (readFromConsole elements)
    let elements = readElements.Split(" ") |> Array.map int
    let queries = (readFromConsole queries).Split(" ") |> Array.map int
    (queries |> Array.toList, elements |> Array.sort |> Array.rev |> Array.toList)

[<EntryPoint>]
let main argv =
    let inputs = argv |> Array.map int
    match inputs with
    | [|elements;queries|] -> printf "%A\n" (executeCannibals(parseInput elements queries))
    | _ -> printf "Wrong Usage\n"
    0