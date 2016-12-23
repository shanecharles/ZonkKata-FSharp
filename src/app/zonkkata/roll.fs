module ZonkKata.Roll 
let SingleDiePoints = function 1 -> 100 
                             | 5 -> 50 
                             | _ -> 0
let ThreePairsPoints = 750
let StraightPoints = 1000
let ThreeOfAKindPoints = function 1 -> 1000 
                                | n -> n * 100

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
let rollThreePairs = function
                     | [(_,2); (_,2); (_,2)]      -> true
                     | [(2,4); (_,2)]             -> true
                     | [(3,4); (x,2)] when x <> 1 -> true
                     | _                          -> false

let rollThreeOfKind = function (_,c) :: _ -> c >= 3
                             | _          -> false
let rollStraight = List.length >> ((=)6)

let (|ThreePairs|Straight|ThreeOfKind|Points|) dice =
    let grp = GroupAndSortByFreq dice
    if   grp |> rollThreePairs  then ThreePairs
    elif grp |> rollStraight    then Straight 
    elif grp |> rollThreeOfKind then ThreeOfKind (grp |> SumFreqPoints)
    else Points (grp |> SumFreqPoints)

let CalculatePoints : int list -> int = function
    | ThreePairs      -> ThreePairsPoints
    | Straight        -> StraightPoints
    | ThreeOfKind pts -> pts
    | Points pts      -> pts

let PrintPoints = CalculatePoints >> function
                  | 0   -> printfn "Zonk!"
                  | pts -> printfn "You rolled %i points." pts