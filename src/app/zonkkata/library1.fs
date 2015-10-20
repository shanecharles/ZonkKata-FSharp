namespace ZonkKata

module Roll =
    let CalculatePoints d =
        let sorted = d |> List.sort
        match sorted with 
        | [1; 2; 3; 4; 5; 6] -> 1500
        | _                  -> 0
