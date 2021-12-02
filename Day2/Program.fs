open System
open System.IO

let test =
    """forward 5
down 5
forward 8
up 3
down 8
forward 2"""

type Position = { X: int; Y: int }
let originPosition = { Position.X = 0; Y = 0 }

type AimPosition = { X: int; Y: int; Aim: int }
let originAimPosition = { AimPosition.X = 0; Y = 0; Aim = 0 }

let (|Down|Up|Forward|) (instruction: string) =
    let instructionParts = instruction.Split(" ")

    let (direction, steps) =
        (instructionParts.[0], instructionParts.[1] |> int)

    match direction with
    | "down" -> Down steps
    | "up" -> Up steps
    | "forward" -> Forward steps
    | _ -> failwith "Absurd entry"

let followInstruction acc instruction =
    match instruction with
    | Down steps -> { acc with Position.Y = acc.Y + steps }
    | Up steps -> { acc with Y = acc.Y - steps }
    | Forward steps -> { acc with X = acc.X + steps }

let followAimInstruction acc instruction =
    match instruction with
    | Down steps ->
        { acc with
              AimPosition.Aim = acc.Aim + steps }
    | Up steps -> { acc with Aim = acc.Aim - steps }
    | Forward steps ->
        { acc with
              X = acc.X + steps
              Y = acc.Y + (acc.Aim * steps) }

[<EntryPoint>]
let main argv =
    let finalPos =
        File.ReadAllLines("/Users/calebbarde/RiderProjects/AdventOfCode2021/Day2/Day2.txt")
        |> Array.toList
        // |> List.fold followInstruction originPosition // part 1
        |> List.fold followAimInstruction originAimPosition // part 2

    let answer = finalPos.X * finalPos.Y
    printfn $"The answer is %i{answer}"
    0 // return an integer exit code
