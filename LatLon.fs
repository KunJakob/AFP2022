namespace Opgave1.Coordinates

open System

module LatLon =

    type LatLon(lat: float, long: float, ele: float) =
        member this.Lat: float = lat
        member this.Lon: float = long
        member this.Ele: float = ele

        member this.print =
            Console.WriteLine("Lat: {0}, Lon: {1}, Ele: {2}", this.Lat, this.Lon, this.Ele)

    let createLatLon (coords: string) : LatLon option =
        let myCoords = coords.Split(';')

        if (myCoords.Length < 2
            || myCoords[0] = ""
            || myCoords[1] = "") then
            None
        else
            let lonEleSplit = myCoords[ 1 ].Split('-')

            //set elevation to 0 if not specified
            if (lonEleSplit.Length = 0) then
                None
            elif (lonEleSplit.Length = 1 && lonEleSplit[0] <> "") then
                Some(LatLon(float myCoords[0], float lonEleSplit[0], 0.0))
            else
                Some(LatLon(float myCoords[0], float lonEleSplit[0], float lonEleSplit[1]))


    let distance (a: LatLon, b: LatLon) : float =
        let lat_a = a.Lat
        let lat_b = b.Lat
        let long_a = a.Lon
        let long_b = b.Lon
        let r = 6371.0
        let p = 0.017453292519943295

        let a =
            0.5 - cos ((lat_b - lat_a) * p) / 2.0
            + cos (lat_a * p)
              * cos (lat_b * p)
              * (1.0 - cos ((long_b - long_a) * p))
              / 2.0

        2.0 * r * asin (sqrt (a))
