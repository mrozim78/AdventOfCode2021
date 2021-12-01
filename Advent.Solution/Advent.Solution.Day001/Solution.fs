namespace Advent.Solution.Day001

open System.IO
open Advent.Solution.Day

type Solution() =
    
    let readNumbers fileName =
        File.ReadAllLines(fileName)
        |> Array.map int
        |> Array.toList
    let step (state:Option<int>*int,element:int)=
        let previous , counter = state
        Some(element), if previous.IsSome && previous.Value<element then counter+1 else counter
    interface ISolution with
        member this.Solve() =
            readNumbers("../../../../../data/Day001/data.txt")
            |> List.fold (fun a b ->  Some(b), if fst(a).IsSome && fst(a).Value<b then snd(a)+1 else snd(a)) (Option<int>.None ,0)
            |> snd
            |> string
        member this.Day() =
            1
       
