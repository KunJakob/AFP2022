open System
open Opgave3.Actor
open Opgave3.Actor.OutputActor
open Opgave3.Actor.InputActor
open Opgave3.Actor.AccountsActor

open Opgave3.Actor
open Akka.FSharp

[<EntryPoint>]
let main _ =
    let actorSystem =
        System.create "ActorSystem" (Configuration.load ())

    spawn actorSystem "outputActor" (outputActor)
    |> ignore

    let accountsActor =
        spawn actorSystem "accountsActor" (accountsActor)

    let inputActor =
        spawn actorSystem "inputActor" (inputActor (accountsActor))


    //This could really use some argument validation, but ¯\_(ツ)_/¯
    let rec inputLoop () =
        let input = Console.ReadLine().Split(" ")

        match input[0] with
        | "createAccount" -> inputActor <! InputMessages.CreateAccount input[1]
        | "createProject" ->
            inputActor
            <! InputMessages.CreateProject(input[1], input[2])
        | "createTodo" ->
            inputActor
            <! InputMessages.CreateTodo(input[1], input[2], input[3], input[4])
        | "updateTodo" ->
            inputActor
            <! InputMessages.UpdateTodo(input[1], input[2], input[3], input[4])
        | "finishTodo" ->
            inputActor
            <! InputMessages.FinishTodo(input[1], input[2], input[3])
        | "listTodo" ->
            inputActor
            <! InputMessages.ListTodos(input[1], input[2])
        | _ -> inputLoop ()

        inputLoop ()

    inputLoop ()

    0
