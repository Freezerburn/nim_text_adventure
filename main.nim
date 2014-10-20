import lists
import tables
import hashes
import strutils
import sets
import actors
import os

const
    NUM_THREADS = 3

    startRoomName = "StartRoom"
    startRoomDescription = "A simple room."

    eastRoomName = "EastRoom"
    eastRoomDescription = "A room to the east of a simple room."

type
    Direction = enum
        DirNorth, DirSouth, DirEast, DirWest,
        DirUp, DirDown,
        DirOut, DirIn

    MessageKind = enum
        MessageQuit,
        MessageLog
    Message = object
        case kind: MessageKind
        of MessageLog:
            msg: string
            newline: bool
        of MessageQuit: nil

    PRoom = ref Room
    Room = object of TObject
        name*: string
        description*: string
        directions: array[DirNorth..DirIn, PRoom]
        linked: TSet[PRoom]

    OptionKind = enum
        SomeKind, NoneKind
    Option[T] = object
        case o: OptionKind
        of SomeKind:
            value: T
        else:
            nil
    EOptionFail = object of E_Base

    ThreadArg = array[NUM_THREADS, ptr TChannel[Message]]

template playerChan: expr = channels[0][]
template tickChan: expr = channels[1][]
template logChan: expr = channels[2][]

proc newRoom(name, description: string): PRoom

proc some[T](v: T): Option[T] =
    return Option[T](o: SomeKind, value: v)

proc none[T](): Option[T] =
    return Option[T](o: NoneKind)

proc isSome[T](opt: Option[T]): bool =
    case opt.o
    of SomeKind: return true
    of NoneKind: return false

proc unwrap[T](opt: Option[T]): T =
    case opt.o
    of SomeKind: return opt.value
    of NoneKind: raise newException(EOptionFail, "Tried to unwrap a none.")

proc log(msg: string): Message =
    return Message(kind: MessageLog, msg: msg, newline: false)

proc logLine(msg: string): Message =
    return Message(kind: MessageLog, msg: msg, newline: true)

proc threadQuit(): Message =
    return Message(kind: MessageQuit)

proc toDirection(d: string): Option[Direction] =
    case d
    of "e", "east": return some(DirEast)
    of "w", "west": return some(DirWest)
    of "n", "north": return some(DirNorth)
    of "s", "south": return some(DirSouth)
    of "o", "out": return some(DirOut)
    of "i", "in": return some(DirIn)
    of "u", "up": return some(DirUp)
    of "d", "down": return some(DirDown)
    else: return none[Direction]()

proc hash(direction: Direction): THash =
    return hash(ord(direction))

proc opposite(direction: Direction): Direction =
    case direction
    of DirNorth: return DirSouth
    of DirSouth: return DirNorth
    of DirEast: return DirWest
    of DirWest: return DirEast
    of DirUp: return DirDown
    of DirDown: return DirUp
    of DirOut: return DirIn
    of DirIn: return DirOut

proc hash(room: PRoom): THash =
    return hash(room.name) + hash(room.description)

proc clone(room: PRoom): PRoom =
    result = newRoom(room.name, room.description)
    result.directions = room.directions
    result.linked = room.linked

proc `$`(dirArr: array[DirNorth..DirIn, PRoom]): string =
    return repr(dirArr)

proc `$`(room: PRoom): string =
    var tmp = Room(
        name: room.name,
        description: room.description,
        linked: initSet[PRoom]()
    )
    for pair in room.directions.pairs():
        var dir = pair.key
        var other = pair.val
        if other.linked.contains(room):
            var stripRoom = clone(room)
            stripRoom.directions[dir] = nil
            stripRoom.linked.excl(room)
            other = stripRoom
        tmp.directions[dir] = other
        tmp.linked.incl(other)
    result = $tmp

proc newRoom(name, description: string): PRoom =
    new(result)
    result.name = name
    result.description = description
    result.linked = initSet[PRoom]()

proc linkRooms(first, second: PRoom, directionToSecond: Direction) =
    first.directions[directionToSecond] = second
    first.linked.incl(second)

    second.directions[opposite(directionToSecond)] = first
    second.linked.incl(first)

proc initRooms(): TTable[string, PRoom] =
    result = initTable[string, PRoom]()

    let startRoom = newRoom(startRoomName, startRoomDescription)
    result[startRoomName] = startRoom 

    let eastRoom = newRoom(eastRoomName, eastRoomDescription)
    result[eastRoomName] = eastRoom

    let southRoom = newRoom("SouthRoom", "A room to the south of the simple room.")
    result["SouthRoom"] = southRoom

    linkRooms(startRoom, eastRoom, DirEast)
    linkRooms(startRoom, southRoom, DirSouth)

proc logThreadFunc(channels: ThreadArg) {.thread.} =
    var going = true
    while going:
        let msg = logChan.recv()
        case msg.kind:
        of MessageLog:
            if msg.newline:
                echo(msg.msg)
            else:
                stdout.write(msg.msg)
                stdout.flushFile()
        of MessageQuit:
            going = false
        sleep(20)

proc tickThreadFunc(channels: ThreadArg) {.thread.} =
    var going = true
    while going:
        let msg = tickChan.recv()
        case msg.kind
        of MessageQuit:
            going = false
        else:
            discard

proc playerThreadFunc(channels: ThreadArg) {.thread.} =
    let rooms = initRooms()
    var currentRoom = rooms[startRoomName]

    var going = true
    var printDescription = true
    while going:
        if printDescription:
            printDescription = false
            echo(currentRoom.description)

        logChan.send(log("Exits are: "))
        var directionsToWrite = ""
        for direction in low(currentRoom.directions)..high(currentRoom.directions):
            if currentRoom.directions[direction] != nil:
                directionsToWrite.add(($direction)[3.. -1] & ", ")
        logChan.send(logLine(directionsToWrite[0.. -3]))

        logChan.send(log(">>>> "))
        let line = readLine(stdin).strip()
        let toProcess = line.split(" ")

        if len(toProcess) > 0:
            case toProcess[0]
            of "exit":
                going = false
            of "go":
                if len(toProcess) > 1:
                    let maybeDir = toProcess[1].toDirection()
                    if maybeDir.isSome():
                        let direction: Direction = unwrap(maybeDir)
                        if currentRoom.directions[direction] != nil:
                            let room = currentRoom.directions[direction]
                            currentRoom = room
                            printDescription = true
                        else:
                            logChan.send(logLine("There are no exist in that direction."))
            else:
                logChan.send(logLine("I do not understand that command."))

        if line == "exit":
            going = false

    tickChan.send(Message(kind: MessageQuit))
    logChan.send(Message(kind: MessageQuit))

when isMainModule:
    var
        actualPlayerChan: TChannel[Message]
        actualTickChan: TChannel[Message]
        actualLogChan: TChannel[Message]

        channels: array[NUM_THREADS, ptr TChannel[Message]]

        playerThread: TThread[array[NUM_THREADS, ptr TChannel[Message]]]
        tickThread: TTHread[array[NUM_THREADS, ptr TChannel[Message]]]
        logThread: TTHread[array[NUM_THREADS, ptr TChannel[Message]]]

    open(actualPlayerChan)
    open(actualTickChan)
    open(actualLogChan)

    channels[0] = addr(actualPlayerChan)
    channels[1] = addr(actualTickChan)
    channels[2] = addr(actualLogChan)

    createThread(playerThread, playerThreadFunc, channels)
    createThread(tickThread, tickThreadFunc, channels)
    createThread(logThread, logThreadFunc, channels)

    joinThreads(playerThread, tickThread, logThread)
