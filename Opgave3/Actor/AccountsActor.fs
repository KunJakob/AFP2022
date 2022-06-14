module Opgave3.Actor.AccountsActor

open Akka.Actor
open Akka.FSharp
open Opgave3.Actor.OutputActor
open Opgave3.Actor.PrimitiveTypes
open Opgave3.Actor.AccountActor



type AccountsMessages =
    | CreateAccount of AccountId
    | CreateProject of AccountId * ProjectId
    | CreateTodo of AccountId * ProjectId * TodoId * TodoContent
    | UpdateTodo of AccountId * ProjectId * TodoId * TodoContent
    | FinishTodo of AccountId * ProjectId * TodoId
    | ListTodos of AccountId * ProjectId


let accountsActor (mailbox: Actor<AccountsMessages>) =
    let outputActor =
        select "akka://ActorSystem/user/outputActor" mailbox.Context.System

    let initialAccounts: Map<AccountId, IActorRef> =
        Map.empty

    let rec loop (accounts: Map<AccountId, IActorRef>) =
        actor {
            let! message = mailbox.Receive()

            return!
                match message with
                | AccountsMessages.CreateAccount (id) ->
                    let accountExists = accounts.ContainsKey(id)

                    if (accountExists) then
                        outputActor
                        <! OutputMessages.Print($"An account with the id {id} already exists.")

                        loop (accounts)
                    else
                        let accountActor =
                            spawn mailbox.Context id (accountActor id)

                        loop (accounts.Add(id, accountActor))
                | AccountsMessages.CreateProject (accountId, projectId) ->
                    let accountExists =
                        accounts.ContainsKey(accountId)

                    if (accountExists) then
                        accounts.Item(accountId)
                        <! AccountMessages.CreateProject(projectId)

                        loop (accounts)
                    else
                        outputActor
                        <! OutputMessages.Print($"An account with the id {accountId} does not exist.")

                        loop (accounts)

                | AccountsMessages.CreateTodo (accountId, projectId, todoId, todoContent) ->
                    let accountExists =
                        accounts.ContainsKey(accountId)

                    if (accountExists) then
                        accounts.Item(accountId)
                        <! AccountMessages.CreateTodo(projectId, todoId, todoContent)

                        loop (accounts)
                    else
                        outputActor
                        <! OutputMessages.Print($"An account with the id {accountId} does not exist.")

                        loop (accounts)

                | AccountsMessages.UpdateTodo (accountId, projectId, todoId, todoContent) ->
                    let accountExists =
                        accounts.ContainsKey(accountId)

                    if (accountExists) then
                        accounts.Item(accountId)
                        <! AccountMessages.UpdateTodo(projectId, todoId, todoContent)

                        loop (accounts)
                    else
                        outputActor
                        <! OutputMessages.Print($"An account with the id {accountId} does not exist.")

                        loop (accounts)

                | AccountsMessages.FinishTodo (accountId, projectId, todoId) ->
                    let accountExists =
                        accounts.ContainsKey(accountId)

                    if (accountExists) then
                        accounts.Item(accountId)
                        <! AccountMessages.FinishTodo(projectId, todoId)

                        loop (accounts)
                    else
                        outputActor
                        <! OutputMessages.Print($"An account with the id {accountId} does not exist.")

                        loop (accounts)

                | AccountsMessages.ListTodos (accountId, projectId) ->
                    let accountExists =
                        accounts.ContainsKey(accountId)

                    if (accountExists) then
                        accounts.Item(accountId)
                        <! AccountMessages.ListTodos(projectId)

                        loop (accounts)
                    else
                        outputActor
                        <! OutputMessages.Print($"An account with the id {accountId} does not exist.")

                        loop (accounts)

        }

    loop (initialAccounts)
