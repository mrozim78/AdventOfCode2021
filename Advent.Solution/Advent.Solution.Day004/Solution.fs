namespace Advent.Solution.Day004

open System
open System.IO
open Advent.Solution.Day

type BingoMark = class
   val Mark:bool
   val Value: int
   new (mark,value)=
   {
      Mark = mark;
      Value = value;
   }
end

type Solution() =
     let readGameAndBingoTables fileName =
        let gameRaw=
            File.ReadAllLines(fileName)
            |> Array.toList
        
        let game = gameRaw.[0].Split(",") |> Array.toList |> List.map(fun map-> map |> int)
       
        let bingoTables =
           gameRaw.GetSlice(Some(1),None)           
           |> List.where(fun a->not(String.IsNullOrEmpty(a)))
           |> List.map(fun a->a.Replace("  "," ").Trim().Split(" ") |> Array.toList)
           |> List.map(fun a->a |> List.map(fun b->BingoMark(false,b |> int)))
           |> List.collect(fun a->a)
        game , bingoTables
     
     let changeGameState(bingoMarks:BingoMark list , number:int) =
         bingoMarks |> List.map(fun a -> if a.Value = number then BingoMark(true,a.Value) else BingoMark(a.Mark ,a.Value))
        
     let isAllMark(bingoMarks:BingoMark list)=
      let filter =
         bingoMarks
         |> List.where(fun a->a.Mark)
      filter.Length = 5
            
     let isWinnerOnTableByRow(bingoMarks:BingoMark list)=
          bingoMarks
         |> List.chunkBySize 5
         |> List.where(fun a->isAllMark(a))
         |> List.length >0
         
     let transposeBingTable(bingoMarks:BingoMark list)=
         bingoMarks
         |> List.chunkBySize 5
         |> List.transpose
         |> List.collect(fun a->a)
         
     let isWinnerOnTable(bingoMarks:BingoMark list) =
       let isBingoInRow = isWinnerOnTableByRow(bingoMarks)
       if (isBingoInRow)
       then
         true
       else                 
         transposeBingTable bingoMarks
         |> isWinnerOnTableByRow                 
         
     let findWinnerInRound(bingoMarks:BingoMark list) =
            bingoMarks
            |> List.chunkBySize 25
            |> List.where(fun a-> isWinnerOnTable(a))
         
     let rec findWinner(result:BingoMark list , game: int list , index:int)=
        let resultNew = changeGameState(result,game.[index])
        let winners = findWinnerInRound(resultNew)
        if (winners.Length>0)
        then
           winners.[0],index
        else
           findWinner(resultNew,game,index+1)
     
     let isTheSomeTable(firstTable:BingoMark list , secondTable:BingoMark list) =
        firstTable
        |> List.zip secondTable
        |> List.where(fun a-> fst(a).Value = snd(a).Value)
        |> List.length=25;
                           
     let rec findSubstractWinner(winners:BingoMark list list , previousWinners:BingoMark list list , index:int)=
        let isNewTable=
            previousWinners
            |> List.where( fun a-> isTheSomeTable(a,winners.[index]))
            |> List.length = 0;
        if (isNewTable)
        then
           winners.[index]
        else
           findSubstractWinner(winners,previousWinners,index+1)
     let rec findLastWinner(result:BingoMark list, game: int list , previousWinners:BingoMark list list , index:int)=
        let resultNew = changeGameState(result,game.[index])
        let winners = findWinnerInRound(resultNew)
        if (winners.Length= result.Length/25)
        then
           findSubstractWinner(winners,previousWinners,0),index
        else
           findLastWinner(resultNew,game,winners, index+1)
     let countSolve(table:BingoMark list, game:int list, index:int)=
       let sum =
         table
         |> List.where(fun a->not a.Mark)
         |> List.map(fun a->a.Value)
         |> List.sum
       sum*game.[index]|> string
     let solver =
        let game,bingoTables = readGameAndBingoTables ("../../../../../data/Day004/data.txt")
        let winner , indexWinner = findWinner(bingoTables,game,0)
        let lastWinner , indexLastWinner = findLastWinner(bingoTables,game,List.empty<BingoMark list> ,0)
        countSolve(winner,game,indexWinner), countSolve(lastWinner,game,indexLastWinner)
     interface ISolution with
        member this.Solve() =
           let solveOne , solveTwo = solver
           $"[ Part 1 value {solveOne} ] [ Part 2 value {solveTwo} ]"
        member this.Day() =
          4
            

