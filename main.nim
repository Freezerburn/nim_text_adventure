import lists
import tables
import hashes
import strutils
import sets
import actors
import os

const
    NUM_THREADS = 4
    PLAYER_THREAD_INDEX = 0
    TICK_THREAD_INDEX = 1
    LOG_THREAD_INDEX = 2
    STATE_THREAD_INDEX = 3

    startRoomName = "StartRoom"
    startRoomDescription = "A simple room."

    eastRoomName = "EastRoom"
    eastRoomDescription = "A room to the east of a simple room."

    playerName = "ThePlayer"
    playerDescription = "The player of this game."
    playerStart = startRoomName

    shopkeepName = "Shopkeeper"
    shopkeepDescription = "A wandering shopkeeper. Maybe you can buy something from him?"
    shopkeepStart = eastRoomName

type
    Direction = enum
        DirNorth, DirSouth, DirEast, DirWest,
        DirUp, DirDown,
        DirOut, DirIn

    LogLevel = enum
        LevelDebug,
        LevelInfo,
        LevelWarning,
        LevelError,
        LevelCritical,
        LevelNone
    MessageKind = enum
        MessageAck,

        MessageLog,

        # State thread messages.
        MoveEntity,
        PrintRoom,
        PrintRoomAtEnt,
        DescribeRoom,
        DescribeRoomAtEnt,
        DescribeEnt,

        MessageQuit
    Message = object
        ackChan: int
        case kind: MessageKind
        of MessageLog:
            msg: string
            newline: bool
            level: LogLevel
        of MoveEntity:
            meEnt: string
            meDir: Direction
        of PrintRoom:
            prLoc: string
        of PrintRoomAtEnt:
            pratEnt: string
        of DescribeRoom:
            drLoc: string
        of DescribeRoomAtEnt:
            dratEnt: string
        of DescribeEnt:
            deEnt: string
            deLoc: string
        of MessageQuit: nil
        else: nil

    PRoom = ref Room
    Room = object of TObject
        name*: string
        description*: string
        directions: array[DirNorth..DirIn, PRoom]
        linked: TSet[PRoom]
        ents: TDoublyLinkedList[PEntity]

    PEntity = ref Entity
    Entity = object of TObject
        name: string
        description: string
        id: int
        location: string

    OptionKind = enum
        SomeKind, NoneKind
    Option[T] = object
        case o: OptionKind
        of SomeKind:
            value: T
        else:
            nil
    EOptionFail = object of E_Base

    ChannelType = TChannel[Message]
    PChannelType = ptr ChannelType
    ThreadArg = array[NUM_THREADS, PChannelType]
    GameThread = TTHread[ThreadArg]

template playerChan: expr = channels[0][]
template tickChan: expr = channels[1][]
template logChan: expr = channels[2][]
template stateChan: expr = channels[3][]

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

template log(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: false, level: LevelNone))

template logLine(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: true, level: LevelNone))

template logDebug(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: false, level: LevelDebug))

template logDebugLine(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: true, level: LevelDebug))

template logInfo(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: false, level: LevelInfo))

template logInfoLine(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: true, level: LevelInfo))

template logWarning(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: false, level: LevelWarning))

template logWarningLine(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: true, level: LevelWarning))

template logError(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: false, level: LevelError))

template logErrorLine(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: true, level: LevelError))

template logCritical(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: false, level: LevelCritical))

template logCriticalLine(msgStr: string) =
    logChan.send(Message(ackChan: NUM_THREADS, kind: MessageLog, msg: msgStr, newline: true, level: LevelCritical))

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

proc `$`(ent: PEntity): string =
    return $(ent[])

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
    result.ents = initDoublyLinkedList[PEntity]()

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

proc initEntities(rooms: var TTable[string, PRoom]): TTable[string, PEntity] =
    result = initTable[string, PEntity]()
    var
        player: PEntity
        shopkeep: PEntity

    new(player)
    player.name = playerName
    player.description = playerDescription
    player.location = playerStart
    result[player.name] = player

    new(shopkeep)
    shopkeep.name = shopkeepName
    shopkeep.description = shopkeepDescription
    shopkeep.location = shopkeepStart
    result[shopkeep.name] = shopkeep

    rooms[player.location].ents.append(player)
    rooms[shopkeep.location].ents.append(shopkeep)

proc logThreadFunc(channels: ThreadArg) {.thread.} =
    var going = true
    while going:
        let msg = logChan.recv()
        case msg.kind:
        of MessageLog:
            let pre = case msg.level
                of LevelDebug: "[DEBUG] "
                else: ""
            stdout.write(pre)
            if msg.newline:
                echo(msg.msg)
            else:
                stdout.write(msg.msg)
                stdout.flushFile()
        of MessageQuit:
            going = false
        else:
            discard

        if msg.ackChan < NUM_THREADS:
            channels[msg.ackChan][].send(Message(kind: MessageAck))

proc stateThreadFunc(channels: ThreadArg) {.thread.} =
    var rooms = initRooms()
    var ents = initEntities(rooms)

    var going = true
    while going:
        let msg = stateChan.recv()
        echo($msg)
        case msg.kind:
        of PrintRoomAtEnt:
            if ents.hasKey(msg.pratEnt):
                let ent = ents[msg.pratEnt]
                let room = rooms[ent.location]
                log("Exits are: ")
                var directionsToWrite = ""
                for direction in low(room.directions)..high(room.directions):
                    if room.directions[direction] != nil:
                        directionsToWrite.add(($direction)[3.. -1] & ", ")
                logLine(directionsToWrite[0.. -3])
            else:
                logWarningLine("Could not find the given entity: " & msg.pratEnt)
        of MessageQuit:
            going = false
        else:
            discard

        if msg.ackChan < NUM_THREADS:
            channels[msg.ackChan][].send(Message(kind: MessageAck))

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
    var going = true
    var printDescription = true
    while going:
        # if printDescription:
        #     printDescription = false
        #     echo(currentRoom.description)

        stateChan.send(Message(ackChan: PLAYER_THREAD_INDEX, kind: PrintRoomAtEnt, pratEnt: playerName))
        discard stateChan.recv()

        log(">>>>  ")
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
                        # if currentRoom.directions[direction] != nil:
                        #     let room = currentRoom.directions[direction]
                        #     currentRoom = room
                        #     printDescription = true
                        # else:
                        #     logLine("There are no exits in that direction.")
            else:
                logLine("I do not understand that command.")

        if line == "exit":
            going = false

    tickChan.send(Message(kind: MessageQuit))
    logChan.send(Message(kind: MessageQuit))
    stateChan.send(threadQuit())

when isMainModule:
    var
        actualPlayerChan: TChannel[Message]
        actualTickChan: TChannel[Message]
        actualLogChan: TChannel[Message]
        actualStateChan: TChannel[Message]

        channels: array[NUM_THREADS, ptr TChannel[Message]]

        playerThread: TThread[array[NUM_THREADS, ptr TChannel[Message]]]
        tickThread: TTHread[array[NUM_THREADS, ptr TChannel[Message]]]
        logThread: TTHread[array[NUM_THREADS, ptr TChannel[Message]]]
        stateThread: GameThread

    open(actualPlayerChan)
    open(actualTickChan)
    open(actualLogChan)
    open(actualStateChan)

    channels[0] = addr(actualPlayerChan)
    channels[1] = addr(actualTickChan)
    channels[2] = addr(actualLogChan)
    channels[3] = addr(actualStateChan)

    createThread(playerThread, playerThreadFunc, channels)
    createThread(tickThread, tickThreadFunc, channels)
    createThread(logThread, logThreadFunc, channels)
    createThread(stateThread, stateThreadFunc, channels)

    joinThreads(playerThread, tickThread, logThread, stateThread)
