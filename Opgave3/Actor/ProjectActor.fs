module Opgave3.Actor.ProjectActor

open Akka.FSharp
open Opgave3.Actor.OutputActor
open Opgave3.Actor.PrimitiveTypes

type ProjectMessages =
    | CreateTodo of TodoId * TodoContent
    | UpdateTodo of TodoId * TodoContent
    | FinishTodo of TodoId
    | ListTodos

let projectActor (id: TodoId) (mailbox: Actor<ProjectMessages>) =
    let outputActor =
        select "akka://ActorSystem/user/outputActor" mailbox.Context.System

    let initialProjects: Map<TodoId, TodoContent> =
        Map.empty

    let rec loop (todos: Map<TodoId, TodoContent>) =
        actor {
            let! message = mailbox.Receive()

            return!
                match message with
                | ProjectMessages.CreateTodo (id, todoContent) ->
                    let todoExists = todos.ContainsKey(id)

                    if (todoExists) then
                        outputActor
                        <! OutputMessages.Print($"A todo with the id {id} already exists.")

                        loop (todos)
                    else
                        loop (todos.Add(id, todoContent))
                | ProjectMessages.UpdateTodo (id, newContent) ->
                    if (todos.ContainsKey(id)) then
                        loop (todos.Add(id, newContent))
                    else
                        outputActor
                        <! OutputMessages.Print($"A todo with the id {id} does not exist.")

                        loop (todos)
                | ProjectMessages.FinishTodo (id) ->
                    if (todos.ContainsKey(id)) then
                        loop (todos.Remove(id))
                    else
                        outputActor
                        <! OutputMessages.Print($"A todo with the id {id} does not exist.")

                        loop (todos)
                | ProjectMessages.ListTodos ->
                    todos
                    |> Map.iter (fun id todoContent ->
                        outputActor
                        <! OutputMessages.Print($"{id}: {todoContent}"))

                    loop (todos)

        }

    loop (initialProjects)
