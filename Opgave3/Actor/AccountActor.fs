module Opgave3.Actor.AccountActor

open Akka.Actor
open Akka.FSharp
open Opgave3.Actor.OutputActor
open Opgave3.Actor.PrimitiveTypes
open Opgave3.Actor.ProjectActor

type AccountMessages =
    | CreateProject of ProjectId
    | CreateTodo of ProjectId * TodoId * TodoContent
    | UpdateTodo of ProjectId * TodoId * TodoContent
    | FinishTodo of ProjectId * TodoId
    | ListTodos of ProjectId

let accountActor (id: AccountId) (mailbox: Actor<AccountMessages>) =
    let outputActor =
        select "akka://ActorSystem/user/outputActor" mailbox.Context.System

    let initialProjects: Map<ProjectId, IActorRef> =
        Map.empty

    let rec loop (projects: Map<ProjectId, IActorRef>) =
        actor {
            let! message = mailbox.Receive()

            return!
                match message with
                | AccountMessages.CreateProject (id) ->
                    let projectExists = projects.ContainsKey(id)

                    if (projectExists) then
                        outputActor
                        <! OutputMessages.Print($"A project with the id {id} already exists.")

                        loop (projects)
                    else
                        let projectActor =
                            spawn mailbox.Context id (projectActor id)

                        loop (projects.Add(id, projectActor))
                | AccountMessages.CreateTodo (projectId, todoId, todoContent) ->
                    let projectExists =
                        projects.ContainsKey(projectId)

                    if (projectExists) then
                        let projectActor = projects.Item(projectId)

                        projectActor
                        <! ProjectMessages.CreateTodo(todoId, todoContent)

                        loop (projects)
                    else
                        outputActor
                        <! OutputMessages.Print($"A project with the id {projectId} does not exist.")

                        loop (projects)

                | AccountMessages.UpdateTodo (projectId, todoId, todoContent) ->
                    let projectExists =
                        projects.ContainsKey(projectId)

                    if (projectExists) then
                        let projectActor = projects.Item(projectId)

                        projectActor
                        <! ProjectMessages.UpdateTodo(todoId, todoContent)

                        loop (projects)
                    else
                        outputActor
                        <! OutputMessages.Print($"A project with the id {projectId} does not exist.")

                        loop (projects)

                | AccountMessages.FinishTodo (projectId, todoId) ->

                    let projectExists =
                        projects.ContainsKey(projectId)

                    if (projectExists) then

                        let projectActor = projects.Item(projectId)

                        projectActor

                        <! ProjectMessages.FinishTodo(todoId)

                        loop (projects)

                    else

                        outputActor

                        <! OutputMessages.Print($"A project with the id {projectId} does not exist.")

                        loop (projects)

                | AccountMessages.ListTodos (projectId) ->
                    let projectExists =
                        projects.ContainsKey(projectId)

                    if (projectExists) then

                        let projectActor = projects.Item(projectId)

                        projectActor

                        <! ProjectMessages.ListTodos

                        loop (projects)

                    else

                        outputActor

                        <! OutputMessages.Print($"A project with the id {projectId} does not exist.")

                        loop (projects)

        }

    loop (initialProjects)
