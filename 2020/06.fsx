open System.IO

// Add answers from the same group to a set and carry over total of each group's set
let foldTotalAnswers setAndTotal (answers: string) = 
    let currSet,total = setAndTotal 
    match answers with 
    | "" -> (Set.empty, total)
    | _ -> 
            let expandedSet = Seq.fold (fun s c -> Set.add c s) (Set.empty) answers |> Set.union currSet
            let total = total + (Set.count expandedSet) - (Set.count currSet)
            (expandedSet, total)

let sumGroups = 
    File.ReadLines("2020/inputs/06.txt")
    |> Seq.fold foldTotalAnswers (Set.empty, 0)

// Repeatedly intersect the answers from the same group and carry over totals
let foldSameAnswers setAndTotal (answers: string) = 
    let currSet,total,isNew = setAndTotal 
    match answers with 
    | "" -> printfn "%O" currSet
            (Set.empty, total + Set.count currSet, true)
    | _ -> 
            let newSet = Seq.fold (fun s c -> Set.add c s) (Set.empty) answers
            match isNew with 
            | true ->   (newSet, total, false)
            | false -> 
                        let intersection = Set.intersect currSet newSet
                        (intersection, total, false)

let sumOfSameAnswers = 
    File.ReadLines("2020/inputs/06.txt")
    |> Seq.fold foldSameAnswers (Set.empty, 0, true)
    |> fun s -> let set,total,isNew = s 
                total + Set.count set
