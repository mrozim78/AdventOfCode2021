namespace Advent.Solution.Day001

open System.IO
open Advent.Solution.Day

type Solution() =
    
    let readNumbers fileName =
        File.ReadAllLines(fileName)
        |> Array.map int
        |> Array.toList
        
    let stepCountNumberOfLargerThenPrevious (state:Option<int>*int,element:int)=
        let previousElement, counter = state
        Some(element), if previousElement.IsSome && previousElement.Value<element then counter+1 else counter
    
    let countNumberOfLargerThenPrevious(numbers:int list)=
        numbers
        |> List.fold (fun state element -> stepCountNumberOfLargerThenPrevious (state,element)) (Option<int>.None ,0)
        |> snd
               
    let listToTripleSumList(numbers:int list)=
        seq {0..1..numbers.Length-3}
        |> Seq.map(fun index ->  numbers.GetSlice(Some(index),None) |> Seq.take 3 |> Seq.sum)
        |> Seq.toList
        
    
    let solver =
        let data = readNumbers("../../../../../data/Day001/data.txt")
        let partOne =
            data
            |> countNumberOfLargerThenPrevious
        let partTwo =
            data
            |> listToTripleSumList
            |> countNumberOfLargerThenPrevious
        partOne,partTwo
        
    interface ISolution with
        member this.Solve() =
           let solveOne , solveTwo = solver
           $"[ Part 1 value {solveOne} ] [ Part 2 value {solveTwo} ]"
        member this.Day() =
            1
       
