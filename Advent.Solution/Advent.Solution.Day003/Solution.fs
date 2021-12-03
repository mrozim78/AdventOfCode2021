namespace Advent.Solution.Day003

open System
open System.IO
open Advent.Solution.Day
type OneBitStat = class
    val PositiveBits:int
    val NegativeBits:int
    new (positiveBits,negativeBits) as this =
        {PositiveBits = positiveBits; NegativeBits = negativeBits;}
end
type BitStatState = class
    val OneBitStats:OneBitStat[]
    new (startSize:int)  =  {
        OneBitStats = seq {0..1..startSize-1}
        |> Seq.map(fun a -> OneBitStat(0,0))
        |> Seq.toArray
    }
    new (oneBitStats:OneBitStat[])  =
    {OneBitStats = oneBitStats} 
end

type Solution() =
     
     let DoOneBitState(numberChar:char,oneBitStat:OneBitStat)=
       if numberChar='1' then OneBitStat(oneBitStat.PositiveBits+1,oneBitStat.NegativeBits) 
          else OneBitStat(oneBitStat.PositiveBits,oneBitStat.NegativeBits+1)
      
     let DoState(state: BitStatState, number:string)=           
       let oneBitStats =
        number
        |> Seq.toList
        |> List.zip (state.OneBitStats |> Array.toList)
        |> List.map(fun a-> DoOneBitState(snd(a),fst(a)))
        |> List.toArray
        
       BitStatState(oneBitStats)
     let GetByteString(state:BitStatState) =
       
       let chars =
            state.OneBitStats
            |> Array.map(fun oneBitStat -> if   oneBitStat.PositiveBits >=oneBitStat.NegativeBits then   '1' else '0')
       String.Join("",chars)
     
     let GetByteNegativeString(state:BitStatState) =
       let  chars =
            state.OneBitStats
            |> Array.map(fun oneBitStat -> if  oneBitStat.PositiveBits >= oneBitStat.NegativeBits && oneBitStat.NegativeBits>0 then '0' else '1')
       String.Join("",chars) 
     
     let GetIntFromByteString(byteString:string)=
       Convert.ToInt32(byteString,2)
     
     let readNumbers fileName =
        File.ReadAllLines(fileName)
        |> Array.toList
     
     let rec FilterData(list:string list , filterElement:string ,  isNegative:bool , position:int)=
        
        let data =
            list
            |> List.where(fun a -> a.StartsWith(filterElement))
        
        let result =
            data
            |> List.fold (fun state number -> DoState(state,number) )(BitStatState(data.[0].Length))
        
        if
            data.Length=1 then data.[0]
        else
            let newElementCount = if isNegative then GetByteNegativeString(result) else GetByteString(result)
            let newElement = $"{filterElement}{newElementCount.[position]}"
            FilterData(data,newElement,isNegative,position+1)
     
     
     let solver =
         let data = readNumbers("../../../../../data/Day003/data.txt")
         let result =
            data
            |> List.fold (fun state number -> DoState(state,number) )(BitStatState(data.[0].Length))
         
         let byteString = GetByteString(result)
         let negativeByteString = GetByteNegativeString(result)
         
         let findByteString = FilterData(data,byteString.[0]|> string,false,1)
         let findNegativeByteString = FilterData(data,negativeByteString.[0]|>string,true,1)     
         
         let part1 = GetIntFromByteString(byteString )*GetIntFromByteString(negativeByteString)
         let part2 = GetIntFromByteString(findByteString)*GetIntFromByteString(findNegativeByteString)
        
         part1,part2
     interface ISolution with
        member this.Solve() =
           let solveOne , solveTwo = solver
           $"[ Part 1 value {solveOne} ] [ Part 2 value {solveTwo} ]"
        member this.Day() =
          3