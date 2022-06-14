module Opgave_4.Project

open Opgave_4
open Opgave_4.Task

type Project =
    | Projects of Project list
    | Tasks of Task list


let rec projectUnpacker (project: Project, tasks: Task list) : Task list =
    match project with
    | Tasks t -> List.append tasks t
    | Projects p -> List.fold (fun task project -> projectUnpacker (project, task)) tasks p
