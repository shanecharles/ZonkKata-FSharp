namespace ZonkKata

module Roll =
    let (|ThreePairs|_|) roll = if roll |> Seq.groupBy (id) |> Seq.forall (fun (_,s) -> s |> Seq.length = 2) then Some () else None

    let CalculatePoints d =
        let sorted = d |> List.sort
        match sorted with 
        | [1; 2; 3; 4; 5; 6] -> 1500
        | ThreePairs         -> 750
        | _                  -> 0
