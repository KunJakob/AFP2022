module Opgave1_Test.LatLonTest

open Xunit
open Opgave1.Coordinates.LatLon

[<Fact>]
let ``createLatLon returns None with malformed input string`` () = Assert.True (createLatLon "").IsNone

[<Fact>]
let ``createLatLon returns LatLon with 0 elevation if elevation not specified`` () =
    let latLonStr = "52.3;57.8"
    Assert.True((createLatLon latLonStr).Value.Ele = 0)



[<Theory>]
[<InlineData("52.3;57.8-9", "57.9;52.2-8", 716.86)>]
[<InlineData("52.3;57.8-9", "62.9;52.2-8", 1223.86)>]
[<InlineData("51.3;57.8-9", "56.9;52.2-8", 721.30)>]
let ``Distance calculator returns according to Havershine Formula`` (val1, val2, expected) =
    //coordinates from dataset. Verified result with external coordinate distance calculators
    let l1 = createLatLon (val1)
    let l2 = createLatLon (val2)
    Assert.Equal(distance (l1.Value, l2.Value), expected, 2) //adding tolerance as web calculator is not nearly as precise
