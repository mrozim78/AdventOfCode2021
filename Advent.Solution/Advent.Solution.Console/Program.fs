open Advent.Solution.Day


let solveOneSolution(solution:ISolution):int option * string option =
    Some(solution.Day()) , Some(solution.Solve())

let printSolution(dayString:string):Unit =
    let solution = match dayString with
        | "001" -> solveOneSolution(Advent.Solution.Day001.Solution())
        | "002" -> solveOneSolution(Advent.Solution.Day002.Solution())
        | _ -> None , None
    let (day,value) = solution
    if (day.IsSome && value.IsSome) then
        printfn $"Solution of day {dayString} has solution {value.Value}"
    else
        printfn $"Solution of day {dayString} has no solution"

[<EntryPoint>]
let main _ =
    printfn "Advent of Code 2021"   
    printSolution "001"
    printSolution "002"
    0  