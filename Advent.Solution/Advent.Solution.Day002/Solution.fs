namespace Advent.Solution.Day002

open System
open System.IO
open Advent.Solution.Day

type CommandType =
    | Forward = 1
    | Down = 2
    | Up = 3

type Command = class
    val CommandType:CommandType
    val Param:int
    new (commandType,param) as this =
        {CommandType=commandType; Param=param}
end

type State = class
    val Horizontal:int
    val Depth :int
    val Aim:int
    new (horizontal,depth,aim) as this =
        {Horizontal=horizontal;Depth=depth;Aim=aim}

end
type Solution() =
     
     let DoCommand(state :State, command:Command):State=
        match command.CommandType with
        | CommandType.Forward -> State(state.Horizontal+command.Param,state.Depth,0)
        | CommandType.Down -> State(state.Horizontal,state.Depth+command.Param,0)
        | CommandType.Up -> State(state.Horizontal,state.Depth-command.Param,0)
        | _ -> raise (Exception($"I don't know commandType"))
     let DoCommandWithAim(state: State , command:Command):State=
        match command.CommandType with
        | CommandType.Forward -> State(state.Horizontal+command.Param,state.Depth+state.Aim*command.Param,state.Aim)
        | CommandType.Down -> State(state.Horizontal,state.Depth,state.Aim+command.Param)
        | CommandType.Up -> State(state.Horizontal,state.Depth,state.Aim-command.Param)
        | _ -> raise (Exception($"I don't know commandType")) 
     
     let ToCommand(commandString:string)=
       let splitCommandString= commandString.Split(' ')
       let commandTypeString = splitCommandString.[0]
       let param = splitCommandString.[1]|> int
       match commandTypeString with 
           | "forward" ->  Command(CommandType.Forward,param)
           | "down" -> Command(CommandType.Down,param)
           | "up" -> Command(CommandType.Up,param)
           | _ -> raise (Exception($"I don't know command {commandTypeString}"))
       
     let readCommands fileName =
        File.ReadAllLines(fileName)
        |> Array.toList
        |> List.map ToCommand
     let solver =
         let data = readCommands("../../../../../data/Day002/data.txt")
         let part1 =
            data
            |> List.fold (fun state element ->  DoCommand(state,element) )(State(0,0,0))    
         let part2 =   
            data
            |> List.fold(fun state element ->  DoCommandWithAim(state,element) )(State(0,0,0))
         part1 , part2
     interface ISolution with
        member this.Solve() =
           let solveOne , solveTwo = solver
           $"[ Part 1 value {solveOne.Horizontal*solveOne.Depth} ] [ Part 2 value {solveTwo.Horizontal*solveTwo.Depth} ]"
        member this.Day() =
            2
