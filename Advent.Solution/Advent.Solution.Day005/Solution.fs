namespace Advent.Solution.Day005

open System.IO
open System.Net
open Advent.Solution.Day
type Point= class
    val X:int
    val Y:int
    override this.ToString()=
       $"[X={this.X},Y={this.Y}]"
    new(x,y)= 
    {
       X=x
       Y=y;
    }
  
end

type Line = class
    val StartPoint:Point
    val EndPoint:Point
    override this.ToString()=
        $"<Start point {this.StartPoint}><End point {this.EndPoint}>"
    new(startPoint,endPoint)=
    {
        StartPoint=startPoint
        EndPoint=endPoint;
    }
end

type PointWithFrequency = class
    inherit Point
    val Frequency:int
     override this.ToString()=
        $"[X={this.X},Y={this.Y},Frequency={this.Frequency}]"
    new(x,y,frequency) =
    {
       inherit Point(x,y);
       Frequency=frequency;
    }
end



type Solution() =
      
      let ToPoint(pointString:string)=
          let splitPointString=pointString.Split(",")
          Point(splitPointString.[0]|>int , splitPointString.[1]|>int)
      let ToLine(lineInFile:string)=
       let splitLineInFile = lineInFile.Split("->")
       let startPointString = splitLineInFile.[0].Trim()
       let endPointString = splitLineInFile.[1].Trim()
       Line(ToPoint startPointString ,ToPoint endPointString)
       
      let readLines fileName =
        File.ReadAllLines(fileName)
        |> Array.toList
        |> List.map ToLine
        
        
      let drawPointsByDiagonalLine(line:Line)=
        let signs = 
            if ( line.StartPoint.X< line.EndPoint.X &&  line.StartPoint.Y < line.EndPoint.Y)
            then
                (1,1)
            else
                if (line.StartPoint.X>line.EndPoint.X && line.StartPoint.Y<line.EndPoint.Y)
                then
                    (-1,1)
                else
                    if (line.StartPoint.X<line.EndPoint.X && line.StartPoint.Y>line.EndPoint.Y)
                    then
                        (1,-1)
                    else
                        (-1,-1)
        
        seq{0..1..abs(line.StartPoint.X-line.EndPoint.X)}
        |> Seq.map(fun x->Point(line.StartPoint.X+x*fst(signs),line.StartPoint.Y+x*snd(signs)))
      
      let drawPointByRowsLine(line:Line)=
          let startPoint = if line.StartPoint.X<=line.EndPoint.X then line.StartPoint.X else line.EndPoint.X
          let endPoint = if line.EndPoint.X>=line.StartPoint.X then line.EndPoint.X else line.StartPoint.X 
          seq{startPoint..1..endPoint}
          |> Seq.map(fun a->Point(a,line.StartPoint.Y))
          
      let drawPointByColsLine(line:Line)=
          let startPoint = if line.StartPoint.Y<=line.EndPoint.Y then line.StartPoint.Y else line.EndPoint.Y
          let endPoint = if line.EndPoint.Y>=line.StartPoint.Y then line.EndPoint.Y else line.StartPoint.Y 
          seq{startPoint..1..endPoint}
          |> Seq.map(fun a->Point(line.StartPoint.X,a))
          
      let  solveFrequencySequenceByLine(array:PointWithFrequency[,] , line:Line) =
          if (line.StartPoint.Y=line.EndPoint.Y)
          then
              drawPointByRowsLine line
              |> Seq.iter(fun element -> array.[element.X,element.Y]<-PointWithFrequency(element.X,element.Y,array.[element.X,element.Y].Frequency+1))
              
          else
            if (line.StartPoint.X = line.EndPoint.X)
            then
                drawPointByColsLine line
                |> Seq.iter(fun element -> array.[element.X,element.Y]<-PointWithFrequency(element.X,element.Y,array.[element.X,element.Y].Frequency+1))
            else
                drawPointsByDiagonalLine line
                |> Seq.iter(fun element -> array.[element.X,element.Y]<-PointWithFrequency(element.X,element.Y,array.[element.X,element.Y].Frequency+1))
      let flatArray2D array2D = 
                seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                          for y in [0..(Array2D.length2 array2D) - 1] do 
                              yield array2D.[x, y] }
      let solveFrequencySequence(frequencyTable:PointWithFrequency seq , lines:Line List , xSize:int)=
          let frequencyArray =
            frequencyTable
            |> Seq.chunkBySize xSize
            |> array2D
            
          lines
          |> List.iter(fun a-> solveFrequencySequenceByLine(frequencyArray,a)) 
          
          frequencyArray |> flatArray2D
      
      let solver =
           let data = readLines("../../../../../data/Day005/data.txt")
                     
           let maxX =
               data
               |> List.map(fun a->if a.StartPoint.X>=a.EndPoint.X then a.StartPoint.X else a.EndPoint.X )
               |> List.max
           let maxY =
               data
               |> List.map(fun a->if a.StartPoint.Y>=a.EndPoint.Y then a.StartPoint.Y else a.EndPoint.Y )
               |> List.max
                             
           let frequencyTable =
               seq{0..1..maxX}
               |> Seq.allPairs (seq{0..1..maxY})
               |> Seq.map(fun a->PointWithFrequency(fst(a),snd(a),0))
         
           let dataLineOnlyRowAndColumn =
               data
               |> List.where(fun a-> a.StartPoint.X=a.EndPoint.X || a.StartPoint.Y=a.EndPoint.Y )
          
           let countDangerous =
                solveFrequencySequence(frequencyTable,dataLineOnlyRowAndColumn,maxX+1)
                |> Seq.where(fun a->a.Frequency>=2)
                |> Seq.length
           let countAllDangerous =
               solveFrequencySequence(frequencyTable,data,maxX+1)
               |> Seq.where(fun a->a.Frequency>=2)
               |> Seq.length
           countDangerous,countAllDangerous
      interface ISolution with
        member this.Solve() =
           let solveOne , solveTwo = solver
           $"[ Part 1 value {solveOne} ] [ Part 2 value {solveTwo} ]"
        member this.Day() =
          5