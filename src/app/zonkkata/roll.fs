module ZonkKata.Roll 
let SingleDiePoints = function | 1 -> 100 | 5 -> 50 | _ -> 0
let ThreePairsPoints = 750
let ThreeOfAKindPoints = function 1 -> 1000 | n -> n * 100

let GroupAndSortByFreq = Seq.groupBy id
                         >> Seq.map (fun (x,s) -> x, s |> Seq.length)
                         >> Seq.sortBy snd
                         >> Seq.toList
                         >> List.rev

let FreqPoints = function
                 | x, c when c > 3 -> x |> ThreeOfAKindPoints |> (*) (c - 2)
                 | x, 3            -> x |> ThreeOfAKindPoints
                 | x, c            -> x |> SingleDiePoints |> (*) c
let SumFreqPoints = List.map FreqPoints >> List.sum
let (|RolledStraight|_|) = Seq.length >> function | 6 -> Some () | _ -> None
// Score more points by breaking up four of a kind of 2s or 3s and another pair, except 3s and pair of 1s.
let (|RolledThreePairs|_|) = function
                             | [(_,2); (_,2); (_,2)]      -> Some ()
                             | [(2,4); (_,2)]             -> Some ()
                             | [(3,4); (x,2)] when x <> 1 -> Some ()
                             | _                          -> None

let CalculatePoints (roll : int list) = roll 
                                        |> GroupAndSortByFreq
                                        |> function
                                        | RolledStraight   -> 1000
                                        | RolledThreePairs -> ThreePairsPoints
                                        | grpd             -> grpd |> SumFreqPoints

let PrintPoints = CalculatePoints
                  >> function
                  | 0   -> printfn "Zonk!"
                  | pts -> printfn "You rolled %i points." pts
