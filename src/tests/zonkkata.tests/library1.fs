namespace ZonkKata.Tests

open System
open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

module Common = 
    let rand = new Random()

    let removeNth l n = l 
                        |> List.fold (fun (nth, i, acc) elem -> 
                                        let acc' = match nth = i with 
                                                   | true -> acc
                                                   | _    -> elem :: acc
                                        (nth, (i+1), acc')) (n, 0, [])
                        |> (fun (_,_,l') -> l' |> List.rev)

    let zonkRoll () = 
        let pool = [2; 2; 3; 3; 4; 4; 6; 6]
        let rec nextDie rem avail =
            match rem with 
            | 1 -> avail 
                |> Seq.groupBy (id) 
                |> Seq.toArray 
                |> Array.sortBy (fun (x,s) -> s |> Seq.length) 
                |> Array.rev
                |> Seq.take 1 
                |> Seq.map (fun (x,_) -> x) 
                |> Seq.toList
            | _ -> 
                let i = rand.Next(avail |> List.length)
                let n = i |> List.nth avail
                n :: (nextDie (rem-1) (removeNth avail i))
        nextDie 6 pool

type ZonkRoll = 
    static member Roll() =
        let g = gen {
            return Common.zonkRoll ()
        }
        g 
        |> Arb.fromGen

type ZonkPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<ZonkRoll> |])
                    
type RoyalRoll = 
    static member Roll() =
        let g = gen {
            let order = [1 .. 5]
                        |> List.rev
                        |> List.map (fun x -> Common.rand.Next(x))

            let rec reorder o (l : int list) =
                match o with 
                | [] -> l
                | h :: o' ->
                    let n = List.nth l h
                    let l' = l |> List.filter (fun x -> x <> n)
                    n :: (reorder o' l')

            return [1 .. 6]
                    |> reorder order
        }
        g |> Arb.fromGen

type RoyalRollPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<RoyalRoll> |])

module BigRoller =
    [<Fact>]
    let ``A roll should return points greater than or equal to zero.`` () =
        let expected = 0
        let actual = [1; 1; 1; 1; 1; 1] |> ZonkKata.Roll.CalculatePoints
        test <@ expected <= actual @>

    [<RoyalRollProperty>]
    let ``A roll of 1 through 6 should return 1500 points.`` (roll : int list) =
        let expected = 1500
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<ZonkProperty>]
    let ``A roll with no points should return zero points.`` (roll : int list) =
        let expected = 0
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<ZonkProperty>]
    let ``A zonk roll should always have 4 unique digits.`` (roll : int list) =
        let expected = 4
        let actual = roll |> Seq.distinct |> Seq.length
        test <@ expected = actual @>

    [<ZonkProperty>]
    let ``A zonk roll should contain no ones.`` (roll : int list) =
        let actual = roll |> Seq.filter (fun x -> x = 1)
        test <@ actual |> Seq.isEmpty @>

    [<ZonkProperty>]
    let ``A zonk roll should contain no fives.`` (roll : int list) =
        let actual = roll |> Seq.filter (fun x -> x = 5)
        test <@ actual |> Seq.isEmpty @>
