namespace Opgave1

open System
open System.IO

module main =

    let maxX = 10
    let maxY = 10

    type Direction =
        | NE
        | SE
        | SW
        | NW
        | N
        | S
        | E
        | W

    let parseDirection (str: string) =
        let str = str.ToUpper()

        match str with
        | "NE" -> NE
        | "SE" -> SE
        | "SW" -> SW
        | "NW" -> NW
        | "N" -> N
        | "S" -> S
        | "E" -> E
        | "W" -> W
        | _ -> raise (InvalidDataException("Invalid direction"))

    type Obstacle(x: int, y: int) =
        member this.X = x
        member this.Y = y

    type Placement(x: int, y: int, dir: Direction, obstacles: Obstacle []) =
        member this.X = x
        member this.Y = y
        member this.Direction = dir
        member this.Obstacles = obstacles


    type Command =
        | FORWARD
        | BACKWARD
        | TURN_LEFT
        | TURN_RIGHT

    //If x or y has reached maxX or maxY, move to opposite side of the grid
    let checkReachedBorder (place: Placement) =
        let mutable newX = place.X
        let mutable newY = place.Y
        if (place.X > maxX) then newX <- 0
        if (place.Y > maxY) then newY <- 0
        if (place.X < 0) then newX <- maxX
        if (place.Y < 0) then newY <- maxY
        Placement(newX, newY, place.Direction, place.Obstacles)

    let obsCollPredicate (place: Placement, obs: Obstacle) = obs.X = place.X && obs.Y = place.Y

    //check if collide with obstacle
    let checkCollision (place: Placement) =
        place.Obstacles
        |> Seq.filter (fun obs -> obsCollPredicate (place, obs))
        |> Seq.length > 0


    //move forward in direction of current Direction and return new placement
    let moveForward (placement: Placement) =
        match placement.Direction with
        | N -> Placement(placement.X, placement.Y + 1, placement.Direction, placement.Obstacles)
        | S -> Placement(placement.X, placement.Y - 1, placement.Direction, placement.Obstacles)
        | E -> Placement(placement.X + 1, placement.Y, placement.Direction, placement.Obstacles)
        | W -> Placement(placement.X - 1, placement.Y, placement.Direction, placement.Obstacles)
        | NE -> Placement(placement.X + 1, placement.Y + 1, placement.Direction, placement.Obstacles)
        | SE -> Placement(placement.X + 1, placement.Y - 1, placement.Direction, placement.Obstacles)
        | SW -> Placement(placement.X - 1, placement.Y - 1, placement.Direction, placement.Obstacles)
        | NW -> Placement(placement.X - 1, placement.Y + 1, placement.Direction, placement.Obstacles)

    let moveBackward (placement: Placement) =
        match placement.Direction with
        | N -> Placement(placement.X, placement.Y - 1, placement.Direction, placement.Obstacles)
        | S -> Placement(placement.X, placement.Y + 1, placement.Direction, placement.Obstacles)
        | E -> Placement(placement.X - 1, placement.Y, placement.Direction, placement.Obstacles)
        | W -> Placement(placement.X + 1, placement.Y, placement.Direction, placement.Obstacles)
        | NE -> Placement(placement.X - 1, placement.Y - 1, placement.Direction, placement.Obstacles)
        | SE -> Placement(placement.X - 1, placement.Y + 1, placement.Direction, placement.Obstacles)
        | SW -> Placement(placement.X + 1, placement.Y + 1, placement.Direction, placement.Obstacles)
        | NW -> Placement(placement.X + 1, placement.Y - 1, placement.Direction, placement.Obstacles)

    let turnLeft (placement: Placement) =
        match placement.Direction with
        | N -> Placement(placement.X, placement.Y, NW, placement.Obstacles)
        | S -> Placement(placement.X, placement.Y, SE, placement.Obstacles)
        | E -> Placement(placement.X, placement.Y, NE, placement.Obstacles)
        | W -> Placement(placement.X, placement.Y, SW, placement.Obstacles)
        | NE -> Placement(placement.X, placement.Y, N, placement.Obstacles)
        | SE -> Placement(placement.X, placement.Y, E, placement.Obstacles)
        | SW -> Placement(placement.X, placement.Y, S, placement.Obstacles)
        | NW -> Placement(placement.X, placement.Y, W, placement.Obstacles)

    let turnRight (placement: Placement) =
        match placement.Direction with
        | N -> Placement(placement.X, placement.Y, NE, placement.Obstacles)
        | S -> Placement(placement.X, placement.Y, SW, placement.Obstacles)
        | E -> Placement(placement.X, placement.Y, SE, placement.Obstacles)
        | W -> Placement(placement.X, placement.Y, NW, placement.Obstacles)
        | NE -> Placement(placement.X, placement.Y, E, placement.Obstacles)
        | SE -> Placement(placement.X, placement.Y, S, placement.Obstacles)
        | SW -> Placement(placement.X, placement.Y, W, placement.Obstacles)
        | NW -> Placement(placement.X, placement.Y, N, placement.Obstacles)

    let move (placement: Placement, command: Command) =
        let newPlacement =
            match command with
            | FORWARD -> moveForward placement
            | BACKWARD -> moveBackward placement
            | TURN_LEFT -> turnLeft placement
            | TURN_RIGHT -> turnRight placement

        checkReachedBorder newPlacement |> ignore

        if (checkCollision newPlacement) then
            Console.WriteLine($"Collision on X: {newPlacement.X} Y: {newPlacement.Y}")
            placement

        else
            newPlacement

    let parseCommand (x: string) : Command =
        match x.ToLower() with
        | "w" -> FORWARD
        | "s" -> BACKWARD
        | "a" -> TURN_LEFT
        | "d" -> TURN_RIGHT
        | _ -> raise (InvalidDataException "Invalid command")




    let initialPrompt () =
        printf "Enter Preconditions <x> <y> <cardinal-direction>\n"
        Console.ReadLine().Trim()

    let rand = Random()

    let generateRandomObstacle _ =
        let x = rand.Next(0, maxX)
        let y = rand.Next(0, maxY)
        Console.WriteLine($"Obstacle at X: {x} Y: {y}")
        Obstacle(x, y)

    let parsePreconditions (x: string) =
        let args = x.Split(' ')

        let obstacles =
            Array.init 10 generateRandomObstacle

        Placement(int args[0], int args[1], parseDirection (args[2]), obstacles)

    let printPlace (place: Placement) =
        Console.WriteLine($"X: {place.X} Y: {place.Y} Direction: {string place.Direction}\n")

    let input _ : Command =
        printf "Enter one of W A S D\n"
        parseCommand (Console.ReadLine().Trim())

    let step (place: Placement, cmd: Command) : Placement =
        let newPlace = move (place, cmd)
        printPlace newPlace
        newPlace

    let loop (initialPlacement: Placement) =
        let inputs = Seq.initInfinite input

        inputs
        |> Seq.fold (fun place cmd -> step (place, cmd)) initialPlacement




    [<EntryPoint>]
    let main _ =
        let initial =
            parsePreconditions (initialPrompt ())

        loop initial |> ignore
        0
