open System
open System.IO

let test =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

type Cell =
    | Unmarked of int
    | Marked of int

type Row =
    | One
    | Two
    | Three
    | Four
    | Five

type Column =
    | A
    | B
    | C
    | D
    | E

type Square = (int * int)
type Board = Map<Square, Cell>

let getVal cell =
    match cell with
    | Unmarked x -> x
    | Marked x -> x

let isMarked cell =
    match cell with
    | Marked _ -> true
    | Unmarked _ -> false

let checkIfWinning board =
    let getRow square = square |> fst
    let getCol square = square |> snd

    let winningCondition aggregator =
        [ 0; 1; 2; 3; 4 ]
        |> List.map (fun idx -> Map.filter (fun k _ -> (aggregator k) = idx) board)
        |> List.map Map.toList
        |> List.map (List.map snd)
        |> List.exists (List.forall isMarked)

    let rowsBingo = winningCondition getRow
    let colsBingo = winningCondition getCol

    rowsBingo || colsBingo


let rec findWinningTurns (move: int) (nums: int list) (board: Board) =
    match nums with
    | [] -> None
    | num :: rest ->
        let MaybeIdx =
            Map.tryFindKey (fun _ v -> (getVal v) = num) board

        match MaybeIdx with
        | Some idx ->
            let valueToUpdate = Map.find idx board |> getVal
            let newBoard = Map.add idx (Marked valueToUpdate) board

            if checkIfWinning newBoard then
                Some(newBoard, move)
            else
                findWinningTurns (move + 1) rest newBoard
        | None -> findWinningTurns (move + 1) rest board




let stringToBoard (boardString: string) =
    let buildRow (acc: Board) (rowIdx: int, rowString: string) =
        let vals =
            rowString.Split(" ")
            |> Array.filter (fun elem -> elem.Equals("") |> not)
            |> Array.map int
            |> Array.indexed

        Array.fold (fun acc (colIdx, value) -> Map.add (rowIdx, colIdx) (Unmarked value) acc) acc vals

    boardString.Split("\n")
    |> Array.indexed
    |> Array.fold buildRow (Board [])


[<EntryPoint>]
let main argv =
    let numbersString :: boardsStrings =
        File
            .ReadAllText("/Users/calebbarde/RiderProjects/AdventOfCode2021/Day4/Day4.txt")
            .Split("\n\n")
        |> Array.toList

    let numbers =
        numbersString.Split(",")
        |> Array.map int
        |> Array.toList

    let bingoBoard =
        boardsStrings
        |> List.map stringToBoard
        |> List.map (findWinningTurns 0 numbers)
        |> List.filter Option.isSome
        |> List.map Option.get
        // List.minBy snd // part 1
        |> List.maxBy snd

    let winningNumber = bingoBoard |> snd

    let unmarkedSum =
        bingoBoard
        |> fst
        |> Map.toList
        |> List.map snd
        |> List.filter (isMarked >> not)
        |> List.sumBy getVal

    printf "bingo: %i" (unmarkedSum * List.item winningNumber numbers)
    0 // return an integer exit code
