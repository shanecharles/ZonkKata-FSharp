namespace ZonkKata.Tests

open System
open Xunit
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

module Common = 
    let rand = new Random()

    let excludeNumber n = seq { for i in 1..6 do if i <> n then yield i }
    let excludeNumbers ns = seq { for i in 1..6 do if ns |> List.forall (fun x -> x <> i) 
                                                   then yield i }
    let removeNth l n = l 
                        |> List.fold (fun (nth, i, acc) elem -> 
                                        let acc' = match nth = i with 
                                                   | true -> acc
                                                   | _    -> elem :: acc
                                        (nth, (i+1), acc')) (n, 0, [])
                        |> (fun (_,_,l') -> l' |> List.rev)
    
    let getAndRemoveNth l n = (n |> List.nth l, n |> removeNth l)

    let randomizeOrder roll = 
        let rec reorder rem =
            match rem with 
            | [] -> []
            | _  -> let (n,rem') = rand.Next(rem |> List.length) |> getAndRemoveNth rem
                    n :: (reorder rem')
        reorder roll

    let oneOrFiveRoll () =
        let pool = [1 .. 6] @ [1 .. 6]
        let rec nextDie rem avail =
            match rem with 
            | 1 -> avail
                |> Seq.groupBy (id)
                |> Seq.choose (fun (x,s) -> if s |> Seq.length > 1 then Some x else None)
                |> Seq.toList
                |> fun pairs -> match pairs with
                                | [p] -> let dice = p |> excludeNumber |> Seq.toList
                                         [rand.Next(dice |> List.length) |> List.nth dice]
                                | _   -> 
                                    let p = match pairs |> List.filter (fun x -> x = 1 || x = 5) |> List.sortBy (id) with
                                            | [1; 5] -> [1; 5]
                                            | _      -> pairs
                                    [rand.Next(p |> List.length) |> List.nth p]
            | _ -> let (n, avail') = rand.Next(avail |> Seq.length) |> getAndRemoveNth avail
                   n :: (nextDie (rem-1) avail')
        nextDie 6 pool

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

type FourOfAKindWithNoExtraPoints =
    static member Roll() =
        let g = gen {
            let! n = Gen.choose (1,6)
            let pool = [n; 1; 5] |> Common.excludeNumbers |> Seq.toList
            let len = (pool |> Seq.length) - 1
            let! i1 = Gen.choose (0, len)
            let! i2 = Gen.choose (0, len)
            let roll = (i1 |> List.nth pool) :: (i2 |> List.nth pool) :: (List.init 4 (fun _ -> n))
                       |> Common.randomizeOrder
            return n,roll
        }
        g 
        |> Arb.fromGen
        |> Arb.filter (fun (n,r) -> match n with 
                                    | 2 -> r |> Seq.groupBy (id) |> Seq.length > 2
                                    | _ -> true)

type FourOfAKindWithExtraPoints = 
    static member Roll() =
        let g = gen {
            let! n = Gen.choose (1,6)
            let pool = n |> Common.excludeNumber |> Seq.toList
            let scoringDice = [1; 5] |> List.filter (fun x -> x <> n) 
            let! i = Gen.choose (0, (scoringDice |> Seq.length) - 1)
            let oneOrFive = i |> List.nth scoringDice
            let! j = Gen.choose (0,4)
            let last = j |> List.nth (n |> Common.excludeNumber |> Seq.toList)
            let roll = oneOrFive :: last :: (List.init 4 (fun _ -> n))
            return n,roll
        }
        g 
        |> Arb.fromGen
        |> Arb.filter (fun (n,r) -> match n with 
                                    | 2 -> r |> Seq.groupBy (id) |> Seq.length > 2
                                    | _ -> true)

type FiveOfAKindWithNoExtraPoints = 
    static member Roll() =
        let g = gen {
            let! n = Gen.choose (1,6)
            let pool = [n; 1; 5] |> Common.excludeNumbers |> Seq.toList
            let! i = Gen.choose (0, (pool |> Seq.length) - 1)
            let last = i |> List.nth pool
            let roll = last :: (List.init 5 (fun _ -> n)) |> Common.randomizeOrder
            return n,roll
        }
        g |> Arb.fromGen

type FiveOfAKindWithExtraPoints = 
    static member Roll() = 
        let g = gen {
            let! n = Gen.choose (1,6)
            let pool = [1; 5] |> List.filter (fun x -> x <> n)
            let! i = Gen.choose (0, (pool |> Seq.length) - 1)
            let last = i |> List.nth pool
            let roll = last :: (List.init 5 (fun _ -> n)) |> Common.randomizeOrder
            return n,roll
        }
        g |> Arb.fromGen

type ThreeOfAKindWithAnotherThreeOfAKind =
    static member Roll() =
        let g = gen {
            let! i1 = Gen.choose (0,5)
            let! i2 = Gen.choose (0,4)
            let (n, rest) = i1 |> Common.getAndRemoveNth [1 .. 6]
            let m = i2 |> List.nth rest
            let roll = [n; n; n; m; m; m] |> Common.randomizeOrder
            return (n, m, roll)
        }
        g |> Arb.fromGen

type ThreeOfAKindWithNoMorePoints =
    static member Roll() =
        let g = gen {
            let! n = Gen.choose (1,6)
            let pool = [n; 1; 5] |> Common.excludeNumbers |> Seq.toList
            let rest = pool @ pool |> Common.randomizeOrder |> Seq.take 3 |> Seq.toList
            let roll = n :: n :: n :: rest |> Common.randomizeOrder
            return (n, roll)
        } 
        g |> Arb.fromGen

type ThreeOfAKindWithExtraPoints = 
    static member Roll() =
        let g = gen {
            let! n = Gen.choose (1,6)
            let rest = n |> Common.excludeNumber |> Seq.toList
            let pool = rest @ rest |> Common.randomizeOrder |> Seq.take 3 |> Seq.toList
            let roll = n :: n :: n :: pool |> Common.randomizeOrder
            return (n, roll)
        }
        g |> Arb.fromGen
          |> Arb.filter (fun (n, roll) -> 
                            let scoring = [1; 5] |> List.filter (fun x -> x <> n)
                            roll |> List.exists (fun x -> scoring |> List.exists (fun y -> y = x)))

type ZonkRoll = 
    static member Roll() =
        let g = gen {
            return Common.zonkRoll ()
        }
        g |> Arb.fromGen

type ThreePairsRoll =
    static member Roll() =
        let g = gen {
            let choose l = Gen.choose (0 , l |> Seq.length)
            let! i1 = Gen.choose (0, 5)
            let! i2 = Gen.choose (0, 4)
            let! i3 = Gen.choose (0, 3)
            let (n1, l1) = Common.getAndRemoveNth [1 .. 6] i1
            let (n2, l2) = Common.getAndRemoveNth l1 i2
            let n3 = i3 |> List.nth l2

            return [n1; n1; n2; n2; n3; n3]
        }
        g |> Arb.fromGen

type OneOrFiveRoll =
    static member Roll() =
        let g = gen {
            return Common.oneOrFiveRoll()
        }
        g |> Arb.fromGen

type FiveOfAKindWithExtraPointsPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<FiveOfAKindWithExtraPoints> |])

type FiveOfAKindWithNoExtraPointsPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<FiveOfAKindWithNoExtraPoints> |])
        
type FourOfAKindWithExtraPointsPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<FourOfAKindWithExtraPoints> |])

type FourOfAKindWithNoExtraPointsPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<FourOfAKindWithNoExtraPoints> |])

type ThreeOfAKindWithAnotherThreeOfAKindPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<ThreeOfAKindWithAnotherThreeOfAKind> |])

type ThreeOfAKindWithExtraPointsPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<ThreeOfAKindWithExtraPoints> |])

type ThreeOfAKindWithNoMorePointsPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<ThreeOfAKindWithNoMorePoints> |])

type OneOrFiveRollPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<OneOrFiveRoll> |])
        
type ThreePairsRollPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<ThreePairsRoll> |])

type ZonkRollPropertyAttribute () =
    inherit PropertyAttribute (
        Arbitrary = [| typeof<ZonkRoll> |])
                    
type RoyalRoll = 
    static member Roll() =
        let g = gen {
            return [1 .. 6] |> Common.randomizeOrder
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
    let ``A roll of 1 through 6 should return 1000 points.`` (roll : int list) =
        let expected = 1000
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<ZonkRollProperty>]
    let ``A roll with no points should return zero points.`` (roll : int list) =
        let expected = 0
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<ZonkRollProperty>]
    let ``A zonk roll should always have 4 unique digits.`` (roll : int list) =
        let expected = 4
        let actual = roll |> Seq.distinct |> Seq.length
        test <@ expected = actual @>

    [<ZonkRollProperty>]
    let ``A zonk roll should contain no ones.`` (roll : int list) =
        let actual = roll |> Seq.filter (fun x -> x = 1)
        test <@ actual |> Seq.isEmpty @>

    [<ZonkRollProperty>]
    let ``A zonk roll should contain no fives.`` (roll : int list) =
        let actual = roll |> Seq.filter (fun x -> x = 5)
        test <@ actual |> Seq.isEmpty @>

    [<ThreePairsRollProperty>]
    let ``A roll of any three pairs should score 750 points.`` (roll : int list) =
        let expected = 750
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<OneOrFiveRollProperty>]
    let ``A roll with no big points should be more than zero points.`` (roll : int list) =
        let expected = 0
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected < actual @>

    [<OneOrFiveRollProperty>]
    let ``A roll with no big points should count no more than 300 points.`` (roll : int list) =
        let expected = 300
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected >= actual @>

    [<Fact>]
    let ``Three ones should score 1000 points.`` () =
        let expected = 1000
        let actual = 1 |> ZonkKata.Roll.ThreeOfAKindPoints
        test <@ expected = actual @>

    [<Theory>]
    [<InlineData(2)>]
    [<InlineData(3)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    let ``Three of a number other than one should score face value * 100 points.`` n =
        let expected = n * 100
        let actual = n |> ZonkKata.Roll.ThreeOfAKindPoints
        test <@ expected = actual @>

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(3)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    let ``Four of a kind should return 2 times the three of a kind points.`` n =
        let expected = (n |> ZonkKata.Roll.ThreeOfAKindPoints) * 2
        let actual = n |> ZonkKata.Roll.FourOfAKindPoints
        test <@ expected = actual @>

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(3)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    let ``Five of a kind should return 3 times the three of a kind points.`` n =
        let expected = (n |> ZonkKata.Roll.ThreeOfAKindPoints) * 3
        let actual = n |> ZonkKata.Roll.FiveOfAKindPoints
        test <@ expected = actual @>

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(3)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    let ``Six of a kind should return 4 times the three of a kind points.`` n =
        let expected = (n |> ZonkKata.Roll.ThreeOfAKindPoints) * 4
        let actual = n |> ZonkKata.Roll.SixOfAKindPoints
        test <@ expected = actual @>

    [<Fact>]
    let ``A roll with three of a kind of twos and no more points should return 200 points.`` () =
        let expected = 200
        let actual = [2; 2; 2; 3; 4; 6] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<Fact>]
    let ``A with three of a kind of twos and a one should return 300 points.`` () =
        let expected = 300
        let actual = [2; 2; 2; 1; 3; 4] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<ThreeOfAKindWithNoMorePointsProperty>]
    let ``Three of a kind of die N and no more points should return three of a kind points for N.`` ((n : int), (roll : int list)) =
        let expected = n |> ZonkKata.Roll.ThreeOfAKindPoints
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<ThreeOfAKindWithExtraPointsProperty>]
    let ``Three of a kind of dice N which contain an extra one or five should return more than three of a kind point for N.`` ((n : int), (roll : int list)) =
        let expected = n |> ZonkKata.Roll.ThreeOfAKindPoints
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected < actual @>

    [<ThreeOfAKindWithAnotherThreeOfAKindProperty>]
    let ``Three of a kind with another three of a kind should return the sum of the two three of a kind points.`` ((n : int, m : int, roll : int list)) =
        let expected = (n |> ZonkKata.Roll.ThreeOfAKindPoints) + (m |> ZonkKata.Roll.ThreeOfAKindPoints)
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<Fact>]
    let ``Four of a kind of ones should return 2000 points.`` () =
        let expected = 2000
        let actual = [1; 1; 1; 1; 2; 3] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<FourOfAKindWithNoExtraPointsProperty>]
    let ``Four of a kind with no extra points should return two times the three of a kind points.`` (n : int, roll : int list) =
        let expected = 2 * (n |> ZonkKata.Roll.ThreeOfAKindPoints)
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<FourOfAKindWithExtraPointsProperty>]
    let ``Four of a kind with extra points should return more than the four of a kind points.`` (n : int, roll : int list) =
        let expected = n |> ZonkKata.Roll.FourOfAKindPoints
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected < actual @>

    [<FiveOfAKindWithNoExtraPointsProperty>]
    let ``Five of a kind with no extra points should return three times the three of a kind points.`` (n : int, roll : int list) =
        let expected = 3 * (n |> ZonkKata.Roll.ThreeOfAKindPoints)
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<FiveOfAKindWithExtraPointsProperty>]
    let ``Five of a kind with extra points should return more than the five of a kind points.`` (n : int, roll : int list) =
        let expected = n |> ZonkKata.Roll.FiveOfAKindPoints
        let actual = roll |> ZonkKata.Roll.CalculatePoints
        test <@ expected < actual @>

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(3)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    let ``Six of a kind should return four times the three of a kind points.`` (n : int) =
        let expected = 4 * (n |> ZonkKata.Roll.ThreeOfAKindPoints)
        let actual = [n; n; n; n; n; n] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<Fact>]
    let ``Four twos and two ones should count more points as three pairs.`` () =
        let expected = 750
        let actual = [2; 2; 2; 2; 1; 1] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>
