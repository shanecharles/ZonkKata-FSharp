namespace ZonkKata

module Roll =
    let singleDiePoints d = match d with
                            | 1 -> 100
                            | 5 -> 50
                            | _ -> 0

    let sumOnesAndFives dice =
        dice |> List.fold (fun acc x -> (x |> singleDiePoints) + acc)  0

    let ThreePairsPoints = 750

    let ThreeOfAKindPoints n =
        match n with 
        | 1 -> 1000
        | _ -> n * 100

    let FourOfAKindPoints n = 2 * (n |> ThreeOfAKindPoints)
    let FiveOfAKindPoints n = 3 * (n |> ThreeOfAKindPoints)
    let SixOfAKindPoints n = 4 * (n |> ThreeOfAKindPoints)

    let (|OfAKind|_|) roll = 
        let getGroupPoints (x, c) =
            match c with 
            | 6 -> x |> SixOfAKindPoints
            | 5 -> x |> FiveOfAKindPoints
            | 4 -> x |> FourOfAKindPoints
            | 3 -> x |> ThreeOfAKindPoints
            | _ -> c * (x |> singleDiePoints)

        roll |> Seq.groupBy (id)
             |> Seq.map (fun (x,s) -> (x, s |> Seq.length))
             |> Seq.sortBy (fun (_,c) -> c)
             |> Seq.toList
             |> List.rev
             |> fun grps -> match grps with 
                            | [(_,2); (_,2); (_,2)]      -> Some ThreePairsPoints
                            | [(2,4); (_,2)]             -> Some ThreePairsPoints
                            | [(3,4); (x,2)] when x <> 1 -> Some ThreePairsPoints
                            | (_,c) :: t when c >= 3 -> 
                                                let pts = grps
                                                          |> List.map getGroupPoints 
                                                          |> List.sum
                                                Some pts
                            | _ -> None

    let CalculatePoints d =
        let sorted = d |> List.sort
        match sorted with 
        | [1; 2; 3; 4; 5; 6] -> 1000
        | OfAKind pts        -> pts
        | _                  -> sorted |> sumOnesAndFives
