namespace ZonkKata

module Roll =
    let (|ThreePairs|_|) roll =
            if roll |> Seq.groupBy (id) |> Seq.forall (fun (_,s) -> s |> Seq.length = 2) then Some (750)
            else None

    let sumOnesAndFives dice =
        dice |> List.fold (fun acc x -> match x with 
                                        | 1 -> 100 + acc
                                        | 5 -> 50 + acc
                                        | _ -> acc) 0

    let ThreeOfAKindPoints n =
        match n with 
        | 1 -> 1000
        | _ -> n * 100

    let FourOfAKindPoints n = 2 * (n |> ThreeOfAKindPoints)

    let FiveOfAKindPoints n = 3 * (n |> ThreeOfAKindPoints)

    let SixOfAKindPoints n = 4 * (n |> ThreeOfAKindPoints)

    let CalculatePoints d =
        let sorted = d |> List.sort
        match sorted with 
        | [1; 2; 3; 4; 5; 6] -> 1000
        | ThreePairs points  -> points
        | _                  -> sorted |> sumOnesAndFives
