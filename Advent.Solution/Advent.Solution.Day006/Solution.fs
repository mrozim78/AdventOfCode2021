namespace Advent.Solution.Day006

open System.Collections.Generic


open System.Collections.Generic
open System.IO
open Advent.Solution.Day

type Solution() =
     let readNumbers fileName =
         File.ReadAllLines(fileName)
         |> Array.toList
         |> List.map(fun a->a.Split(",") |> Array.toList)
         |> List.collect(fun a->a)
         |> List.map(fun a->a |> int)
         
     let createMap(numbers:int list) =
       let seq =
            seq{0..1..8} |>
            Seq.map(fun a->a, numbers |> List.where(fun b->b=a) |> List.length |> bigint )
       Map(seq)
     let dayTick(state:int list)=
         let newLampions =
             state
             |> List.where(fun a->a=0)
             |> List.map(fun a->8)
         let newState =
            state
            |> List.map(fun a-> if a>0 then a-1 else 6)
         
         newState |> List.append newLampions
         
     let dayTickSymbolic(mapNumbers:Map<int,bigint>)=
       let mapDictionary = Dictionary<int,bigint>()
       seq{0..1..8}
            |> Seq.iter(fun a-> mapDictionary.Add(a,0 |>bigint))
       
       seq{0..1..8}
          |> Seq.iter(fun a->
                if a=0
                then
                    mapDictionary.[6]<-mapDictionary.[6]+mapNumbers.[a]
                    mapDictionary.[8]<-mapDictionary.[8]+mapNumbers.[a]
                else
                    mapDictionary.[a-1]<- mapDictionary.[a-1]+mapNumbers.[a]
                )
       let seq = seq{0..1..8} |> Seq.map(fun a-> a , mapDictionary.[a]) 
       Map(seq) 
      
     let getSum(map:Map<int,bigint>)=
        seq{0..1..8}
        |> Seq.map(fun a->map.[a])
        |> Seq.sum
     let solver =
        let number = readNumbers ("../../../../../data/Day006/data.txt")
        let map = createMap(number)
        let partOneDays = 80-1
        let parTwoDays = 256-1
        let partOne =
            seq{0..1..partOneDays}
            |> Seq.fold(fun state list-> dayTickSymbolic(state))(map)
            
        let partTwo =
            seq{0..1..parTwoDays}
            |> Seq.fold(fun state list-> dayTickSymbolic(state))(map)
           
        getSum partOne , getSum partTwo
     interface ISolution with
      member this.Solve() =
        let solveOne , solveTwo = solver
        $"[ Part 1 value {solveOne} ] [ Part 2 value {solveTwo} ]"
      member this.Day() =
        6