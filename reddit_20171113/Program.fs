open System

let rec firstRecurringCharacterIndex (chars:List<char>) (seen:Set<char>) : int =
    match chars with
    | [] -> -1
    | c::_ when (Set.contains c seen) -> Set.count seen
    | c::cs -> firstRecurringCharacterIndex cs (Set.add c seen)

[<EntryPoint>]
let main argv =
    let input = Console.ReadLine()
    let index = (firstRecurringCharacterIndex (Seq.toList input) Set.empty)
    Console.WriteLine(index)
    0 