module MarsRover
type CardinalDirection = North | East | South | West
type RoverState =
| Ready of Rover
| Uninitialised
and Rover = {
    X: int
    Y: int
    Facing: CardinalDirection
}

let initialiseRover() = { X = 0; Y = 0; Facing = North } |> Ready
type Direction = Right | Left
type Command = Turn of Direction
type Instruction = Instruction of Command*RoverState

let (|FaceNorth|FaceEast|FaceSouth|FaceWest|) =
    function
    | Instruction ( Turn dir, Ready(rover) ) when (rover.Facing = West && dir = Right)
            || (rover.Facing = East && dir = Left)
            -> FaceNorth rover
    | Instruction ( Turn dir, Ready(rover) ) when (rover.Facing = North && dir = Right)
            || (rover.Facing = South && dir = Left)
            -> FaceEast rover
    | Instruction ( Turn dir, Ready(rover) ) when (rover.Facing = East && dir = Right)
            || (rover.Facing = West  && dir = Left)
            ->  FaceSouth rover
    | Instruction ( Turn dir, Ready(rover) ) when (rover.Facing = North && dir = Left)
            || (rover.Facing = South && dir = Right)
            -> FaceWest rover
    
type RoverNavigatorBuilder() =
    member __.Bind( x, f ) =
        match x with
        | FaceEast rover -> { rover with Facing = East }
        | FaceSouth rover ->  { rover with Facing = South }
        | FaceWest rover ->  { rover with Facing = West }
        | FaceNorth rover ->  { rover with Facing = North }
        |> Ready
        |> f
        
    member __.Return ( x ) =
        match x with
        | Ready r -> r
        | _ -> failwith "Rover hasn't landed yet!"
        
    member __.Yield _ = { X = 0; Y = 0; Facing = North }
    member __.For _ = { X = 0; Y = 0; Facing = North }
    member __.Zero _ = None
    [<CustomOperation("turnRight")>]
    member __.TurnRight rover =
        let instruction = Instruction( Turn Right, Ready(rover) )
        __.Bind(instruction, __.Return)

    [<CustomOperation("turnLeft")>]
    member __.TurnLeft rover =
        let instruction = Instruction( Turn Left, Ready(rover) )
        __.Bind(instruction, __.Return)

let navigator = RoverNavigatorBuilder()

let getRoverState (marsRover:RoverState) =
    navigator {
        return marsRover
    }
