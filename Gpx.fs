namespace Opgave1.Parsing

open System
open Opgave1.Coordinates.LatLon

module Gpx =
    type TrackSegment(name: string, points: LatLon option []) =
        member this.Name: string = name
        member this.Points: LatLon option [] = points

        member this.TotalDistance: float =
            Array.pairwise (this.Points)
            |> Array.map (fun a ->
                if (fst(a).IsSome && snd(a).IsSome) then
                    distance (fst(a).Value, snd(a).Value)
                else
                    0)
            |> Array.sum

        member this.GainedElevation =
            Array.pairwise (this.Points)
            |> Array.map (fun a ->
                if (fst(a).IsSome && snd(a).IsSome) then
                    if (fst(a).Value.Ele > snd(a).Value.Ele) then
                        fst(a).Value.Ele - snd(a).Value.Ele
                    else
                        0
                else
                    0)
            |> Array.sum

        member this.Print =
            Console.WriteLine(this.Name)

            Console.WriteLine(
                "Gained elevation: "
                + string this.GainedElevation
                + " m"
            )

            Console.WriteLine(
                "Total distance: "
                + string this.TotalDistance
                + " km\n"
            )

    type GpxData(bound1: LatLon, bound2: LatLon, tracks: TrackSegment []) =
        member this.Bound1: LatLon = bound1
        member this.Bound2: LatLon = bound2
        member this.Tracks: TrackSegment [] = tracks

        member this.AvgDistance: float =
            this.Tracks
            |> Array.map (fun a -> a.TotalDistance)
            |> Array.sum
            |> fun (x) -> x / float this.Tracks.Length

        member this.print =
            Console.WriteLine("GpxData: ")
            Console.Write("Bound1: ")
            this.Bound1.print
            Console.Write("Bound2: ")
            this.Bound2.print

            Console.WriteLine(
                "AvgDistance: "
                + string this.AvgDistance
                + " km\n"
            )

            this.Tracks |> Array.map (fun t -> t.Print)

    let parseLatLonList (latlons: string) =
        latlons.Split('#') |> Array.map (createLatLon)

    let parseData (data: string []) =
        let bounds = data[ 0 ].Split('#')
        let bound1 = createLatLon (bounds[0])
        let bound2 = createLatLon (bounds[1])

        let trackSegments =
            data[1..]
            |> Array.chunkBySize 2
            |> Array.map (fun a -> a[0], parseLatLonList (a[1]))
            |> Array.map (fun a -> TrackSegment(fst a, snd a))

        GpxData(bound1.Value, bound2.Value, trackSegments)
