let fileToNumbers filepath = 
    System.IO.File.ReadLines(filepath) 
    |> List.ofSeq 
    |> List.map int32

let expenses = fileToNumbers "2020/inputs/01.txt"

let pairs = seq { for x in expenses do for y in expenses do [x; y] }
let pairs2 = seq { for x in expenses do for y in expenses do for z in expenses do [x; y; z] }

let doSumTo2020 entry = 
    List.reduce ( + ) entry
    |> fun x -> x = 2020

let findResult pairs = 
    Seq.find doSumTo2020 pairs
    |> List.reduce ( * )

let result1 = findResult pairs
let result2 = findResult pairs2