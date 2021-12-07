namespace Advent.Solution.Day007

open System.IO
open Advent.Solution.Day

type Solution() =
    let readNumbers fileName =
         File.ReadAllLines(fileName)
         |> Array.toList
         |> List.map(fun a->a.Split(",") |> Array.toList)
         |> List.collect(fun a->a)
         |> List.map(fun a->a |> int)
    let findRow(data:int list, min:int , max:int)=
        let lengths =
            seq{min..1..max}
            |> Seq.map(fun a-> a,data |> List.map(fun b->if a>=b then a-b else b-a) |> List.sum )
            
        let min = lengths |> Seq.map(fun a->snd(a)) |> Seq.min
        
        let minLengths = lengths |> Seq.where(fun a-> snd(a)=min)|> Seq.toList
        
        snd(minLengths.[0])
        
    let coeff (row:int)=
        row*(row+1)/2 |> int
    let findRowWithCoeff(data:int list, min:int , max:int)=
        let lengths =
            seq{min..1..max}
            |> Seq.map(fun a-> a,data |> List.map(fun b->if a>=b then coeff(a-b) else coeff(b-a)) |> List.sum )
            
        let min = lengths |> Seq.map(fun a->snd(a)) |> Seq.min
        
        let minLengths = lengths |> Seq.where(fun a-> snd(a)=min)|> Seq.toList
        
        snd(minLengths.[0])
    
    let solver =
        let data = readNumbers ("../../../../../data/Day007/data.txt")
        let min = data |> List.min
        let max = data |> List.max
        let part1 = findRow (data,min,max)
        let part2 = findRowWithCoeff (data,min,max) |>string 
        part1,part2
    interface ISolution with
         member this.Solve() =
          let solveOne , solveTwo = solver
          $"[ Part 1 value {solveOne} ] [ Part 2 value {solveTwo} ]"
         member this.Day() =
         7