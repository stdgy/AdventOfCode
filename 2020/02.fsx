open System.Text.RegularExpressions

let count c s = 
    Seq.filter (fun x -> x = c) s 
    |> Seq.length

let validPassword ruleAndPassword = 
    let min, max, c, password = ruleAndPassword
    let charCount = count c password
    charCount >= min && charCount <= max

let altValidPassword ruleAndPassword = 
    let first, second, c, password = ruleAndPassword
    let a = Seq.item (first - 1) password 
    let b = Seq.item (second - 1) password 
    (a = c && b <> c) || (a <> c && b = c)

let parseLine line = 
    let m = Regex.Match(line, "(\d+)-(\d+)\s+(\w):\s+(\w+)")
    (int32 m.Groups.[1].Value, int32 m.Groups.[2].Value, m.Groups.[3].Value.Chars(0), m.Groups.[4].Value)

let getResult passwordFunc filepath = 
    System.IO.File.ReadLines(filepath) 
    |> Seq.map parseLine
    |> Seq.filter passwordFunc 
    |> Seq.length

let result = getResult validPassword "2020/inputs/02.txt"
let result2 = getResult altValidPassword "2020/inputs/02.txt"