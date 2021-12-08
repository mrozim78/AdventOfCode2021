namespace Advent.Solution.Day008

open System.IO
open Advent.Solution.Day

type Line = class
   val Numbers:string list
   val Outputs:string list
   new(numbers,outputs)=
   {
       Numbers=numbers
       Outputs=outputs;
   }
end
type Solution() =
        
   let calculateOutput (patterns, output) =
    //Lookup helpers
    let ofUniqueLength n segments =
        segments
        |> Seq.find (fun s -> s |> Seq.length = n)

    let ofLength n segments =
        segments
        |> Seq.filter (fun s -> s |> Seq.length = n)
    //ADT operations
    let minus a b = Set.difference b a
    let containsSegment = Set.isSubset 

    //Let's figure out which segment pattern represents which digit
    //Start with the easy digits of unique segment lengths
    let one = patterns |> ofUniqueLength 2
    let four = patterns |> ofUniqueLength 4
    let seven = patterns |> ofUniqueLength 3
    let eight = patterns |> ofUniqueLength 7

    //Complex digits need some deduction more deduction, we'll use segments CF and BD to figure them out
    let segmentsCF = one
    let segmentsBD = four |> minus segmentsCF

    let digitsWithFiveSegments = patterns |> ofLength 5

    let three =
        digitsWithFiveSegments
        |> Seq.find (containsSegment segmentsCF)

    let five =
        digitsWithFiveSegments
        |> Seq.find (containsSegment segmentsBD)

    let two =
        digitsWithFiveSegments
        |> Seq.except [ three; five ]
        |> Seq.head

    let digitsWithSixSegments = patterns |> ofLength 6

    let six =
        digitsWithSixSegments
        |> Seq.find (containsSegment segmentsCF >> not)

    let nine =
        digitsWithSixSegments
        |> Seq.filter (containsSegment segmentsCF)
        |> Seq.filter (containsSegment segmentsBD)
        |> Seq.head

    let zero =
        digitsWithSixSegments
        |> Seq.except [ nine; six ]
        |> Seq.head

    //YAY! Let's build a lookup table mapping segments to digits now
    let lookup =
        [ (zero, 0)
          (one, 1)
          (two, 2)
          (three, 3)
          (four, 4)
          (five, 5)
          (six, 6)
          (seven, 7)
          (eight, 8)
          (nine, 9) ]
        |> Map.ofSeq

    let outputDigit =
        output
        |> Seq.map (fun digit -> lookup |> Map.find digit)
        |> Seq.map string
        |> String.concat ""
        |> int

    outputDigit
         
   let readLine(line:string)=
        let lineSplit = line.Trim().Split("|")
        let numbersString = lineSplit.[0].Trim()
        let outputsString = lineSplit.[1].Trim()
        Line(numbersString.Split(" ")|> Array.toList , outputsString.Split(" ") |>Array.toList)
   let readLines fileName=
        File.ReadAllLines(fileName)
        |> Array.toList
        |> List.map(fun a->readLine(a))
   let solver =
       let data = readLines ("../../../../../data/Day008/data.txt")
       let part1 =
         data
         |> List.collect(fun a->a.Outputs)
         |> List.where(fun a-> a.Length = 2 || a.Length=4 || a.Length=3 || a.Length = 7)
         |> List.length
         
       let part2 =
         data 
         |> List.map(fun a-> calculateOutput (a.Numbers |> List.map Set.ofSeq ,a.Outputs |> List.map Set.ofSeq))
         |> List.sum
       part1 , part2
   interface ISolution with         
         member this.Solve() =
           let solveOne , solveTwo = solver
           $"[ Part 1 value {solveOne} ] [ Part 2 value {solveTwo} ]"
         member this.Day() =
            7