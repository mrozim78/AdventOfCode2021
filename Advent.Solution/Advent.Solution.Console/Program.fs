open Advent.Solution.Day


let solveOneSolution(solution:ISolution):int option * string option =
    Some(solution.Day()) , Some(solution.Solve())

let printSolution(dayString:string):Unit =
    let solution = match dayString with
        | "001" -> solveOneSolution(Advent.Solution.Day001.Solution())
        | "002" -> solveOneSolution(Advent.Solution.Day002.Solution())
        | "003" -> solveOneSolution(Advent.Solution.Day003.Solution())
        | "004" -> solveOneSolution(Advent.Solution.Day004.Solution())
        | "005" -> solveOneSolution(Advent.Solution.Day005.Solution())
        | "006" -> solveOneSolution(Advent.Solution.Day006.Solution())
        | "007" -> solveOneSolution(Advent.Solution.Day007.Solution())
        | "008" -> solveOneSolution(Advent.Solution.Day008.Solution())
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
    printSolution "003"
    printSolution "004"
    printSolution "005"
    printSolution "006"
    printSolution "007"
    printSolution "008"
    0  