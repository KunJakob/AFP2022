module Opgave3.Actor.InputActor

open Opgave3.Actor.PrimitiveTypes
open Akka.FSharp.Actors
open Opgave3.Actor.AccountsActor


type InputMessages =
    | CreateAccount of AccountId
    | CreateProject of AccountId * ProjectId
    | CreateTodo of AccountId * ProjectId * TodoId * TodoContent
    | UpdateTodo of AccountId * ProjectId * TodoId * TodoContent
    | FinishTodo of AccountId * ProjectId * TodoId
    | ListTodos of AccountId * ProjectId


let inputActor accountsActor (mailbox: Actor<InputMessages>) =
    let rec loop () =
        actor {
            let! message = mailbox.Receive()

            match message with
            | InputMessages.CreateAccount accountName ->
                accountsActor
                <! AccountsMessages.CreateAccount(accountName)
            | InputMessages.CreateProject (accountId, projectId) ->
                accountsActor
                <! AccountsMessages.CreateProject(accountId, projectId)
            | InputMessages.CreateTodo (accountId, projectId, todoId, todoContent) ->
                accountsActor
                <! AccountsMessages.CreateTodo(accountId, projectId, todoId, todoContent)
            | InputMessages.UpdateTodo (accountId, projectId, newTodoId, todoContent) ->
                accountsActor
                <! AccountsMessages.UpdateTodo(accountId, projectId, newTodoId, todoContent)
            | InputMessages.FinishTodo (accountId, projectId, todoId) ->
                accountsActor
                <! AccountsMessages.FinishTodo(accountId, projectId, todoId)
            | InputMessages.ListTodos (accountId, projectId) ->
                accountsActor
                <! AccountsMessages.ListTodos(accountId, projectId)

            return! loop ()
        }

    loop ()
