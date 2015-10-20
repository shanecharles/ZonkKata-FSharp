namespace ZonkKata.Tests

open System
open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

type RoyalRoll = 
    static member Roll() =
        let g = gen {
            let rand = new Random()
            let order = [1 .. 5]
                        |> List.rev
                        |> List.map (fun x -> rand.Next(x))

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

    [<Fact>]
    let ``A roll with no points should return zero points.`` () =
        let expected = 0
        let actual = [2; 2; 3; 4; 4; 6] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>
