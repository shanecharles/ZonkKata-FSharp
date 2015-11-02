namespace ZonkKata.Tests

open System
open Xunit
open Xunit.Extensions
open Swensen.Unquote
open FsCheck
open FsCheck.Xunit

module Common = 
    let rand = new Random()

    let OfAKind size n = List.init size (fun _ -> n)
    let HasOfAKind n = Seq.groupBy id >> Seq.map (fun (x,s) -> s |> Seq.length) >> Seq.exists (fun x -> x = n)
    let NoPairWithFourOfAKind2or3 = function 2,r | 3,r -> r |> Seq.distinct |> Seq.length > 2 | _ -> true
    let AtLeastDistinct n roll = roll |> Seq.distinct |> Seq.length > n
    let HasExtraPoints = function
                         | 1,r -> r |> Seq.exists (fun x -> x = 5)
                         | 5,r -> r |> Seq.exists (fun x -> x = 1)
                         | _,r -> r |> Seq.exists (fun x -> x = 1 || x = 5)

    let randomizeOrder roll = 
        let rmem = roll |> List.toArray
        let len = rmem.Length
        rmem
        |> Array.iteri (fun i1 _ -> let pos = (len - 1) - i1
                                    let x = rmem.[pos]
                                    let i2 = rand.Next(len - i1)
                                    rmem.[pos] <- rmem.[i2]
                                    rmem.[i2] <- x)
        rmem |> Array.toList

type Die =
    static member Gen = Gen.choose (1,6)
    static member Value () = Die.Gen |> Arb.fromGen

type NonScoringDie = 
    static member Gen = Gen.elements [2; 3; 4; 6]
    static member Value () = NonScoringDie.Gen |> Arb.fromGen

type ScoringDie =
    static member Gen = Gen.elements [1; 5]
    static member Value () = ScoringDie.Gen |> Arb.fromGen

type FourOfAKindWithNoExtraPoints =
    static member Roll() =
        let g = gen {
            let! n = Die.Gen
            let! r = Gen.listOfLength 2 NonScoringDie.Gen
            let roll = r @ (Common.OfAKind 4 n) |> Common.randomizeOrder
            return n,roll
        }
        g 
        |> Arb.fromGen
        |> Arb.filter (fun (n,r) -> (r |> Common.HasOfAKind 4) && ((n,r) |> Common.NoPairWithFourOfAKind2or3))

type FourOfAKindWithExtraPoints = 
    static member Roll() =
        let g = gen {
            let! n = Die.Gen
            let! r = Gen.listOfLength 2 Die.Gen
            let roll = r @ (Common.OfAKind 4 n) |> Common.randomizeOrder
            return n,roll
        }
        g 
        |> Arb.fromGen
        |> Arb.filter Common.HasExtraPoints

type FiveOfAKindWithNoExtraPoints = 
    static member Roll() =
        let g = gen {
            let! n = Die.Gen
            let! r = NonScoringDie.Gen
            let roll = r :: (Common.OfAKind 5 n) |> Common.randomizeOrder
            return n,roll
        }
        g |> Arb.fromGen
          |> Arb.filter (fun (n,r) -> r |> Common.HasOfAKind 5)

type FiveOfAKindWithExtraPoints = 
    static member Roll() = 
        let g = gen {
            let! n = Die.Gen
            let! r = ScoringDie.Gen
            let roll = r :: (Common.OfAKind 5 n) |> Common.randomizeOrder
            return n,roll
        }
        g |> Arb.fromGen

type ThreeOfAKindWithAnotherThreeOfAKind =
    static member Roll() =
        let g = gen {
            let! n = Die.Gen
            let! m = Die.Gen
            let roll = [n; n; n; m; m; m] |> Common.randomizeOrder
            return (n, m, roll)
        }
        g |> Arb.fromGen
          |> Arb.filter (fun (n,m,_) -> if n = m then false else true)

type ThreeOfAKindWithNoMorePoints =
    static member Roll() =
        let g = gen {
            let! n = Die.Gen
            let! r = Gen.listOfLength 3 NonScoringDie.Gen
            let roll = r @ (Common.OfAKind 3 n) |> Common.randomizeOrder
            return (n, roll)
        } 
        g |> Arb.fromGen
          |> Arb.filter (fun (n,r) -> r |> Common.HasOfAKind 3 && r |> Common.AtLeastDistinct 3)

type ThreeOfAKindWithExtraPoints = 
    static member Roll() =
        let g = gen {
            let! n = Die.Gen
            let! r = Gen.listOfLength 3 Die.Gen
            let roll = r @ (Common.OfAKind 3 n) |> Common.randomizeOrder
            return (n, roll)
        }
        g |> Arb.fromGen
          |> Arb.filter Common.HasExtraPoints

type ZonkRoll = 
    static member Roll() =
        let distinct4 = Seq.distinct >> Seq.length >> function 4 -> true | _ -> false
        let lessThan3ofakind = Seq.groupBy id
                               >> Seq.map (fun (x,s) -> x, s |> Seq.length)
                               >> Seq.forall (fun (_,c) -> c < 3)
        // Using Common.AtLeastDistinct causes a stack overflow exception o.O
        Gen.listOfLength 6 NonScoringDie.Gen
        |> Arb.fromGen
        |> Arb.filter (fun r -> (r |> distinct4) && (r |> lessThan3ofakind))

type ThreePairsRoll =
    static member Roll() =
        let g = gen {
            let! n1 = Die.Gen
            let! n2 = Die.Gen
            let! n3 = Die.Gen
            return [n1; n1; n2; n2; n3; n3]
        }
        g |> Arb.fromGen
          |> Arb.filter (Seq.distinct >> Seq.length >> (fun x -> x = 3))

type OneOrFiveRoll =
    static member Roll() =
        let g = gen {
            return! Gen.listOfLength 6 Die.Gen
        }
        let byFreq = Seq.groupBy id >> Seq.map (fun (_,s) -> s |> Seq.length)
        g |> Arb.fromGen
          |> Arb.filter (fun r -> let freq = r |> byFreq
                                  r |> List.exists (fun x -> x = 1 || x = 5)
                                  && freq |> Seq.forall (fun c -> c < 3)
                                  && freq |> Seq.exists (fun c -> c <> 2)
                                  && r |> Seq.distinct |> Seq.length |> (fun c -> c < 6))

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

module CommonTests =
    [<Fact>]
    let ``Passing a list to the randomizer should randomize the list.`` () =
        let actual = [1 .. 6] |> Common.randomizeOrder
        test <@ [1 .. 6] <> actual @>

    [<Fact>]
    let ``Passing a list to the randomizer and sorting should return the same list.`` () =
        let expected = [1 .. 6]
        let actual = expected |> Common.randomizeOrder |> Seq.sortBy id |> Seq.toList
        test <@ expected = actual @>

module BigRoller =
    [<Property(Arbitrary = [| typeof<NonScoringDie> |])>]
    let ``A non scoring die should not contain a 1 or a 5.`` (d : int) =
        [1; 5] |> List.forall (fun x -> x <> d)

    [<Property(Arbitrary = [| typeof<Die> |])>]
    let ``A die value should be between 1 and 6.`` (d : int) =
        d >= 1 && d <= 6

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
        let expected = ZonkKata.Roll.ThreePairsPoints
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

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(3)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    let ``Four twos and any other pair count more points as three pairs.`` (n : int) =
        let expected = ZonkKata.Roll.ThreePairsPoints
        let actual = (n :: n :: [2; 2; 2; 2]) |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<Theory>]
    [<InlineData(2)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    [<InlineData(6)>]
    let ``Four threes and any pair but ones should return three pairs score.`` (n : int) =
        let expected = ZonkKata.Roll.ThreePairsPoints
        let actual = (n :: n :: [3; 3; 3; 3]) |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>

    [<Fact>]
    let ``Four threes and a pair of ones should return 800 points.`` () =
        let expected = 800
        let actual = [1; 1; 3; 3; 3; 3] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>
