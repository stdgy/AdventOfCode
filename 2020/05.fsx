open System.IO

let assignRowCol state letter = 
    let row,col,rowMin,rowMax,colMin,colMax = state 
    let midRow = (rowMin + rowMax) / 2
    let midCol = (colMin + colMax) / 2
    match letter with 
    | 'F' -> (rowMin, col, rowMin, midRow, colMin, colMax) 
    | 'B' -> (rowMax, col, midRow + 1, rowMax, colMin, colMax)  
    | 'R' -> (row, colMax, rowMin, rowMax/2, midCol + 1, colMax) 
    | 'L' -> (row, colMin, rowMin, rowMax/2, colMin, midCol)  
    | _ -> (row, col, rowMin, rowMax, colMin, colMax)

let getSeatId state = 
    let row,col,rowMin,rowMax,colMin,colMax = state 
    (row * 8) + col

let instructions = File.ReadLines("2020/inputs/05.txt")

let seatIds = 
    Seq.map ((fun instruction -> Seq.fold assignRowCol (0, 0, 0, 127, 0, 7) instruction) >> getSeatId) instructions 

let maxId = Seq.max seatIds

let sortedSeatIds = Seq.sort seatIds 

let mySeatId = 
    Seq.zip sortedSeatIds (Seq.tail sortedSeatIds)
    |> Seq.filter (fun e -> (snd e) - (fst e) > 1)
    |> Seq.map (fun e -> (snd e ) - 1)
    |> Seq.head