// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open System.IO
open System

let test =
    """199
200
208
210
200
207
240
269
260
263"""

open System

type CountByIncrease = int -> int list -> int * int list

let rec countByIncrease: CountByIncrease =
    fun acc list ->
        match list with
        | []
        | [ _ ] -> (acc, [])
        | x :: y :: rest when x >= y -> countByIncrease acc (y :: rest)
        | x :: y :: rest when x < y -> countByIncrease (acc + 1) (y :: rest)
        | _ -> failwith "absurd case"

let rec countBySlidingSumIncrease: CountByIncrease =
    fun acc list ->
        match list with
        | []
        | [ _ ]
        | [ _; _ ]
        | [ _; _; _ ] -> (acc, [])
        | x :: y :: z :: d :: rest when x + y + z >= y + z + d -> countBySlidingSumIncrease acc (y :: z :: d :: rest)
        | x :: y :: z :: d :: rest when x + y + z < y + z + d ->
            countBySlidingSumIncrease (acc + 1) (y :: z :: d :: rest)
        | _ -> failwith "absurd case"

[<EntryPoint>]
let main argv =
    // TODO make this less disgusting
    let (answer, _) =
        File.ReadAllLines("/Users/calebbarde/RiderProjects/AdventOfCode2021/AdventOfCode2021/Day1.txt")
        |> Seq.toList
        |> List.map int
        // |> countByIncrease 0 // part 1
        |> countBySlidingSumIncrease 0

    printfn $"the solution is: %i{answer}"
    0 // return an integer exit code
