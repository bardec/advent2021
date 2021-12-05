// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

type Vector =
    | Vertical of int
    | Horizontal of int
    | Diagonal of (int * int)

type Coord =
    | Coord of x: int * y: int
    static member (-)(Coord (x2, y2), Coord (x1, y1)) =
        if x1 = x2 then Vertical(y2 - y1)
        else if y1 = y2 then Horizontal(x2 - x1)
        else Diagonal((x2 - x1), (y2 - y1))

type Segment = Segment of Coord * Vector

let stringToSegment (line: string) =
    let stringToCoord (coordString: string) =
        let coords = coordString.Split(",") |> Array.map int

        Coord(coords.[0], coords.[1])

    let coords =
        line.Split(" -> ") |> Array.map stringToCoord

    Segment(coords.[0], coords.[1] - coords.[0])

let generateCoords (Coord (x0, y0) as startingCoord) vec =
    let rec _generateCoords coords vec =
        match vec with
        | Horizontal 0
        | Vertical 0
        | Diagonal (0, 0) -> coords
        | Horizontal x ->
            let nextX = if x < 0 then x + 1 else x - 1
            _generateCoords (Coord(x0 + x, y0) :: coords) (Horizontal(nextX))
        | Vertical y ->
            let nextY = if y < 0 then y + 1 else y - 1
            _generateCoords (Coord(x0, y0 + y) :: coords) (Vertical(nextY))
        | Diagonal (x, y) ->
            let nextX = if x < 0 then x + 1 else x - 1
            let nextY = if y < 0 then y + 1 else y - 1
            _generateCoords (Coord(x0 + x, y0 + y) :: coords) (Diagonal(nextX, nextY))

    //    part 1
//-------------------
//    match vec with
//    | Diagonal _ -> []
//    | Horizontal _ | Vertical _ -> _generateCoords [startingCoord] vec
    _generateCoords [ startingCoord ] vec

let drawLine board (Segment (coord, vec)) =
    let incValue v =
        match v with
        | Some v -> Some(v + 1)
        | None -> Some 1

    generateCoords coord vec
    |> List.fold (fun acc coord -> Map.change coord incValue acc) board

[<EntryPoint>]
let main argv =
    let overlaps =
        File.ReadAllLines("/Users/calebbarde/RiderProjects/AdventOfCode2021/Day5/Day5.txt")
        |> Array.map stringToSegment
        |> Array.fold drawLine (Map [])
        |> Map.filter (fun _k v -> v > 1)
        |> Map.count

    printfn "There are %A overlaps" overlaps
    0 // return an integer exit code
