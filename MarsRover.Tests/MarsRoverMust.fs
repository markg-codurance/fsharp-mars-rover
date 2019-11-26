module MarsRoverMust

open NUnit.Framework
open MarsRover

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``initialise at 0,0 facing north`` () =
    let rover = initialiseRover()
    let expectedState = { X=0;Y=0;Facing=North }
    let roverState = navigator { return rover }
    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face east after using the navigation system to turn right`` () =
    let expectedState = { X=0;Y=0;Facing=East }
    let roverState = navigator { turnRight }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face south after using the navigation system to turn right while facing east initially`` () =
    let expectedState = { X=0;Y=0;Facing=South }
    let roverState = navigator { turnRight; turnRight }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face west after using the navigation system to turn right after facing south`` () =
    let expectedState = { X=0;Y=0;Facing=West }
    let roverState = navigator { turnLeft; turnLeft; turnRight }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face west after using the navigation system to turn right while facing north initially`` () =
    let expectedState = { X=0;Y=0;Facing=West }
    let roverState = navigator { turnLeft }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face north after using the navigation system to turn right while facing west initially`` () =
    let expectedState = { X=0;Y=0;Facing=North }
    let roverState = navigator { turnLeft; turnRight }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face south after using the navigation system to turn left while facing west initially`` () =
    let expectedState = { X=0;Y=0;Facing=South }
    let roverState = navigator { turnLeft; turnLeft }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face east after using the navigation system to turn left while facing south initially`` () =
    let expectedState = { X=0;Y=0;Facing=East }
    let roverState = navigator { turnLeft; turnLeft; turnLeft }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``face north after using the navigation system to turn left while facing east initially`` () =
    let expectedState = { X=0;Y=0;Facing=North }
    let roverState = navigator { turnRight; turnLeft }

    Assert.AreEqual(expectedState, roverState)

[<Test>]
let ``after moving from origin the coordinates should be (0,1)``() =
    let expectedState = { X=0;Y=1;Facing=North}
    let roverState = navigator { move }
    Assert.AreEqual(expectedState, roverState)
