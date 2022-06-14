namespace Opgave1

open System.IO
open Opgave1.Parsing

module main =

    [<EntryPoint>]
    let main argv =
        let data =
            File.ReadAllLines "../../../gpxData.txt"

        let gpx = Gpx.parseData data
        gpx.print
        0
