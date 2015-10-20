namespace ZonkKata.Tests

open Xunit
open Swensen.Unquote

module BigRoller =
    [<Fact>]
    let ``A roll should return points greater than or equal to zero.`` () =
        let expected = 0
        let actual = [1; 1; 1; 1; 1; 1] |> ZonkKata.Roll.CalculatePoints
        test <@ expected <= actual @>

    [<Fact>]
    let ``A roll of 1 - 6 should return 1500 points.`` () =
        let expected = 1500
        let actual = [1 .. 6] |> ZonkKata.Roll.CalculatePoints
        test <@ expected = actual @>
