// Notes: I think that this could have been solved better with some active patterns coercing to a type value
// I also think looking at C#s binary types might be helpful

// Might be useful to see how to make this more readable, I don't feel it is
open System
open System.IO

let test =
    """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

let findMostCommonBit byteString =
    let groupedBits = byteString |> Array.countBy id

    let onesCount =
        groupedBits
        |> Array.find (fun (x, _) -> x = '1')
        |> snd

    let zerosCount =
        groupedBits
        |> Array.find (fun (x, _) -> x = '0')
        |> snd

    if onesCount >= zerosCount then
        '1'
    else
        '0'

let negateBit bit =
    match bit with
    | '1' -> '0'
    | '0' -> '1'
    |_ -> failwith "should only be 0 or 1"

let negate (binaryArray: char []) = Array.map negateBit binaryArray

let binaryToDecimal (binaryArray: char []) =
    let binaryReducer bit (idx, value) =
        match bit with
        | '1' -> (idx + 1, pown 2 idx + value)
        | '0' -> (idx + 1, value)

    Array.foldBack binaryReducer binaryArray (0, 0)
    |> snd

let partOne =
    let gamma =
        File.ReadAllLines("/Users/calebbarde/RiderProjects/AdventOfCode2021/Day3/Day3.txt")
        |> Array.map (fun s -> s.ToCharArray())
        |> Array.transpose
        |> Array.map findMostCommonBit

    let epsilon = gamma |> negate

    printfn
        "The answer is: %i"
        ((gamma |> binaryToDecimal)
         * (epsilon |> binaryToDecimal))

let rec findRating (idx: int) bitFilter (readOut: char [] []) =
    if Array.length readOut = 1 then
        readOut |> Array.head
    else
        let matchingBit =
            readOut
            |> Array.map (fun byteString -> Array.item idx byteString)
            |> bitFilter

        let remainingElems =
            Array.filter (fun bits -> (Array.item idx bits) = matchingBit) readOut

        findRating (idx + 1) bitFilter remainingElems

let partTwo =
    let readOut =
        File.ReadAllLines("/Users/calebbarde/RiderProjects/AdventOfCode2021/Day3/Day3.txt")
        |> Array.map (fun s -> s.ToCharArray())

    let oxygenGeneratorRating =
        findRating 0 findMostCommonBit readOut
        |> binaryToDecimal

    let carbonDioxideScrubberRating =
        findRating 0 (findMostCommonBit >> negateBit) readOut
        |> binaryToDecimal

    let lifeSupportRating =
        (oxygenGeneratorRating
         * carbonDioxideScrubberRating)

    printfn "The answer is: %i" lifeSupportRating

[<EntryPoint>]
let main argv =
    // partOne
    partTwo
    0 // return an integer exit code
