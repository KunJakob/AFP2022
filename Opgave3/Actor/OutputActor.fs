module Opgave3.Actor.OutputActor

open Akka.FSharp.Actors
type OutputMessages = Print of string

let outputActor (mailbox: Actor<OutputMessages>) =
    let rec loop () =
        actor {
            let! message = mailbox.Receive()

            match message with
            | OutputMessages.Print message -> printfn $"%s{message}\n"

            return! loop ()
        }

    loop ()
