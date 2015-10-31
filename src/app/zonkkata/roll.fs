module ZonkKata.Roll 
let SingleDiePoints = function | 1 -> 100 | 5 -> 50 | _ -> 0
let ThreePairsPoints = 750
let ThreeOfAKindPoints = function 1 -> 1000 | n -> n * 100
let FourOfAKindPoints = ThreeOfAKindPoints >> (*) 2
let FiveOfAKindPoints = ThreeOfAKindPoints >> (*) 3
let SixOfAKindPoints = ThreeOfAKindPoints >> (*) 4

let SumOnesAndFives = Seq.map SingleDiePoints >> Seq.sum
let GroupAndSortByFrequency = Seq.groupBy id
                              >> Seq.map (fun (x,s) -> x, s |> Seq.length)
                              >> Seq.sortBy snd
                              >> Seq.toList
                              >> List.rev

let CalcGroupPoints = function
                     | x, 6 -> x |> SixOfAKindPoints
                     | x, 5 -> x |> FiveOfAKindPoints
                     | x, 4 -> x |> FourOfAKindPoints
                     | x, 3 -> x |> ThreeOfAKindPoints
                     | x, c -> c * (x |> SingleDiePoints)

let (|RolledStraight|_|) = Seq.length >> function | 6 -> Some () | _ -> None
// You score more points by breaking up four of a kind of 2s or 3s and another pair, except 3s and pair of 1s.
let (|RolledThreePairs|_|) = function
                             | [(_,2); (_,2); (_,2)]      -> Some ()
                             | [(2,4); (_,2)]             -> Some ()
                             | [(3,4); (x,2)] when x <> 1 -> Some ()
                             | _                          -> None

let (|RolledNOfAKind|_|) = function
                        | (x,c) :: t when c >= 3 -> (x,c) :: t
                                                    |> List.map CalcGroupPoints
                                                    |> List.sum
                                                    |> Some
                        | _ -> None

let CalculatePoints (roll : int list) =
    match roll |> GroupAndSortByFrequency with
    | RolledStraight     -> 1000
    | RolledThreePairs   -> ThreePairsPoints
    | RolledNOfAKind pts -> pts
    | _                  -> roll |> SumOnesAndFives

let PrintPoints = CalculatePoints
                  >> function
                  | 0   -> printfn "Zonk!"
                  | pts -> printfn "You rolled %i points." pts
