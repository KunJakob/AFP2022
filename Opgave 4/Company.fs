module Opgave_4.Company

open System
open Opgave_4.Employee
open Opgave_4.Project



type Company =
    { name: string
      employees: Employee list
      projects: Project list }
//traverse all projects and all tasks within projects and create a list of task for each employee in the company

let incrementMap (map: Map<_, int>, key) =
    if (map.ContainsKey(key)) then
        map[key] = map[key] + 1 |> ignore
        map
    else
        map


let validAssignedTasks (c: Company, cap: int) : Boolean =
  c.projects
    |> List.fold (fun acc y -> projectUnpacker (y, acc)) []
    |> List.map (fun g -> g.assignee)
    |> List.fold (fun acc n -> incrementMap(acc, n)) Map.empty<Employee, int>
    |> Map.filter (fun k v -> v > cap)
    |> Map.count
    |> (fun x -> x > 0)
