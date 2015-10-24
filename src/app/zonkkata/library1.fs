namespace ZonkKata

module Roll =
    let (|ThreePairs|_|) roll =
            if roll |> Seq.groupBy (id) |> Seq.forall (fun (_,s) -> s |> Seq.length = 2) then Some ()
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

    let FourOfAKindPoints n =
        (n |> ThreeOfAKindPoints) * 2

    let FiveOfAKindPoints n =
        (n |> ThreeOfAKindPoints) * 3

    let CalculatePoints d =
        let sorted = d |> List.sort
        match sorted with 
        | [1; 2; 3; 4; 5; 6] -> 1000
        | ThreePairs         -> 750
        | _                  -> sorted |> sumOnesAndFives
