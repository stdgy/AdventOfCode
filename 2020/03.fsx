
let getMap filepath = 
    System.IO.File.ReadLines(filepath) 
    |> Array.ofSeq
    |> Array.map Array.ofSeq

let map = getMap "2020/inputs/03.txt"

let rec tobaggan pos travelVector = 
    let x,y = pos
    let vx,vy = travelVector
    let width = (Array.length (Array.item 0 map))
    let x2 = (x + vx)
    let y2 = (y + vy) % width
    match x >= Array.length map with 
    | true -> 0 
    | false -> 
        match (Array.item y (Array.item x map)) with 
        | '.' -> 0 + (tobaggan (x2, y2) travelVector)
        | '#' -> 1 + (tobaggan (x2, y2) travelVector)
        | _ -> 0

let n1 = decimal (tobaggan (0,0) (1,1))
let n2 = decimal (tobaggan (0,0) (1,3))
let n3 = decimal (tobaggan (0,0) (1,5))
let n4 = decimal (tobaggan (0,0) (1,7))
let n5 = decimal (tobaggan (0,0) (2,1))

let total = n1 * n2 * n3 * n4 * n5 

let _ = printfn "Part One: %O" n2
let _ = printfn "Part Two: %O" total