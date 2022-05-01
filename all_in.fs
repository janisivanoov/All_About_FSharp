module Integers =
    let sampleInteger = 176

    let sampleInteger2 = (sampleInteger/4 + 5 - 7) * 4

    let sampleNumbers = [ 0 .. 99 ]

    let sampleTableOfSquares = [ for i in 0 .. 99 -> (i, i*i) ]

    printfn "The table of squares from 0 to 99 is:\n%A" sampleTableOfSquares


module BasicFunctions =

    let func1 x = x*x + 3

    let func1a (x) = x*x + 3

    let result1 = func1 4573

    printfn "The result of squaring the integer 4573 and adding 3 is %d" result1

    let func2 (x:int) = 2*x*x - x/5 + 3

    let result2 = func2 (7 + 4)

    printfn "The result of applying the 1st sample function to (7 + 4) is %d" result2

    let func3 x =
        if x < 100.0 then
            2.0*x*x - x/5.0 + 3.0
        else
            2.0*x*x + x/5.0 - 37.0

    let result3 = func3 (6.5 + 4.5)

    printfn "The result of applying the 2nd sample function to (6.5 + 4.5) is %f" result3

module SomeBooleanValues =

    let boolean1 = true

    let boolean2 = false

    let boolean3 = not boolean1 && (boolean2 || false)

    printfn "The expression 'not boolean1 && (boolean2 || false)' is %A" boolean3

module StringManipulation =

    let string1 = "Hello"

    let string2  = "world"

    let string3 = @"c:\Program Files\"

    let string4 = """He said "hello world" after you did"""

    let helloWorld = string1 + " " + string2 

    printfn "%s" helloWorld

    let substring = helloWorld.[0..6]

    printfn "%s" substring

module Tuples =

    let tuple1 = (1, 2, 3)

    let swapElems (a, b) = (b, a)

    printfn "The result of swapping (1, 2) is %A" (swapElems (1,2))

    let tuple2 = (1, "fred", 3.1415)

    printfn "tuple1: %A    tuple2: %A" tuple1 tuple2

module Lists =

    let list1 = [ ]            
    let list2 = [ 1; 2; 3 ]    
    let list3 = 42 :: list2    
    let numberList = [ 1 .. 1000 ]  
    let daysList =
        [ for month in 1 .. 12 do
              for day in 1 .. System.DateTime.DaysInMonth(2012, month) do
                  yield System.DateTime(2012, month, day) ]

    let blackSquares =

        [ for i in 0 .. 7 do
              for j in 0 .. 7 do
                  if (i+j) % 2 = 1 then
                      yield (i, j) ]

    let squares =
        numberList
        |> List.map (fun x -> x*x)

    let sumOfSquaresUpTo n =
        numberList
        |> List.filter (fun x -> x % 3 = 0)
        |> List.sumBy (fun x -> x * x)

module DefiningClasses =
    type Vector2D(dx : float, dy : float) =

        let length = sqrt (dx*dx + dy*dy)

        member this.DX = dx

        member this.DY = dy

        member this.Length = length

        member this.Scale(k) = Vector2D(k * this.DX, k * this.DY)


    let vector1 = Vector2D(3.0, 4.0)

    let vector2 = vector1.Scale(10.0)

    printfn "Length of vector1: %f      Length of vector2: %f" vector1.Length vector2.Length

module DefiningGenericClasses =

    type StateTracker<'T>(initialElement: 'T) = 

        let mutable states = [ initialElement ]

        member this.UpdateState newState =
            states <- newState :: states  

        member this.History = states

        member this.Current = states.Head

    let tracker = StateTracker 10

    tracker.UpdateState 17

type ReadFile() =

    let file = new System.IO.StreamReader("readme.txt")

    member this.ReadLine() = file.ReadLine()

    interface System.IDisposable with
        member this.Dispose() = file.Close()

module Arrays =

    let array1 = [| |]

    let array2 = [| "hello"; "world"; "and"; "hello"; "world"; "again" |]

    let array3 = [| 1 .. 1000 |]

    let array4 = [| for word in array2 do
                        if word.Contains("l") then
                            yield word |]

    let evenNumbers = Array.init 1001 (fun n -> n * 2)

    let evenNumbersSlice = evenNumbers.[0..500]

    for word in array4 do
        printfn "word: %s" word

    array2.[1] <- "WORLD!"

    let sumOfLengthsOfWords =
        array2
        |> Array.filter (fun x -> x.StartsWith "h")
        |> Array.sumBy (fun x -> x.Length)

module Sequences =

    let seq1 = Seq.empty

    let seq2 = seq { yield "hello"; yield "world"; yield "and"; yield "hello"; yield "world"; yield "again" }

    let numbersSeq = seq { 1 .. 1000 }

    let seq3 =
        seq { for word in seq2 do
                  if word.Contains("l") then
                      yield word }

    let evenNumbers = Seq.init 1001 (fun n -> n * 2)

    let rnd = System.Random()

    let rec randomWalk x =
        seq { yield x
              yield! randomWalk (x + rnd.NextDouble() - 0.5) }

    let first100ValuesOfRandomWalk =
        randomWalk 5.0
        |> Seq.truncate 100
        |> Seq.toList

module RecursiveFunctions  =

    let rec factorial n =
        if n = 0 then 1 else n * factorial (n-1)

    let rec greatestCommonFactor a b =
        if a = 0 then b
        elif a < b then greatestCommonFactor a (b - a)
        else greatestCommonFactor (a - b) b

    let rec sumList xs =
        match xs with
        | []    -> 0
        | y::ys -> y + sumList ys

    let rec private sumListTailRecHelper accumulator xs =
        match xs with
        | []    -> accumulator
        | y::ys -> sumListTailRecHelper (accumulator+y) ys

    let sumListTailRecursive xs = sumListTailRecHelper 0 xs

module RecordTypes =
    type ContactCard =
        { Name     : string
          Phone    : string
          Verified : bool }

    let contact1 = { Name = "Alf" ; Phone = "(206) 555-0157" ; Verified = false }

    let contact2 = { contact1 with Phone = "(206) 555-0112"; Verified = true }

    let showCard c =
        c.Name + " Phone: " + c.Phone + (if not c.Verified then " (unverified)" else "")

module UnionTypes =

    type Suit =
        | Hearts
        | Clubs
        | Diamonds
        | Spades

    type Rank =

        | Value of int
        | Ace
        | King
        | Queen
        | Jack

        static member GetAllRanks() =
            [ yield Ace
              for i in 2 .. 10 do yield Value i
              yield Jack
              yield Queen
              yield King ]

    type Card =  { Suit: Suit; Rank: Rank }

    let fullDeck =
        [ for suit in [ Hearts; Diamonds; Clubs; Spades] do
              for rank in Rank.GetAllRanks() do
                  yield { Suit=suit; Rank=rank } ]

    let showCard c =
        let rankString =
            match c.Rank with
            | Ace -> "Ace"
            | King -> "King"
            | Queen -> "Queen"
            | Jack -> "Jack"
            | Value n -> string n

        let suitString =
            match c.Suit with
            | Clubs -> "clubs"
            | Diamonds -> "diamonds"
            | Spades -> "spades"
            | Hearts -> "hearts"

        rankString  + " of " + suitString

    let printAllCards() =
        for card in fullDeck do
            printfn "%s" (showCard card)

module OptionTypes =
    type Customer = { zipCode : decimal option }

    [<AbstractClass>]

    type ShippingCalculator =

        abstract GetState : decimal -> string option

        abstract GetShippingZone : string -> int

        member this.CustomerShippingZone(customer : Customer) =

            customer.zipCode
            |> Option.bind this.GetState
            |> Option.map this.GetShippingZone

module PatternMatching =

    type Person =
        { First : string
          Last  : string }

    type Employee =
        | Engineer  of Person
        | Manager   of Person * list<Employee>
        | Executive of Person * list<Employee> * Employee

    let rec countReports(emp : Employee) =
        1 + match emp with
            | Engineer(id) ->
                0
            | Manager(id, reports) ->
                reports |> List.sumBy countReports
            | Executive(id, reports, assistant) ->
                (reports |> List.sumBy countReports) + countReports assistant

    let rec findDaveWithOpenPosition(emps : Employee list) =
        emps
        |> List.filter(function
                       | Manager({First = "Dave"}, []) -> true       
                       | Executive({First = "Dave"}, [], _) -> true
                       | _ -> false)                                 

module UnitsOfMeasure =

    open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

    [<Measure>]
    type mile =

        static member asMeter = 1600.<meter/mile>


    let d  = 50.<mile>          
    let d' = d * mile.asMeter   

    printfn "%A = %A" d d'

module ParallelArrayProgramming =

    let oneBigArray = [| 0 .. 100000 |]

    let rec computeSomeFunction x =
        if x <= 2 then 1
        else computeSomeFunction (x - 1) + computeSomeFunction (x - 2)

    let computeResults() = oneBigArray |> Array.Parallel.map (fun x -> computeSomeFunction (x % 20))

    printfn "Parallel computation results: %A" (computeResults())

module Events =

    open System

    let simpleEvent = new Event<int>()

    simpleEvent.Publish.Add(fun x -> printfn "this is handler was added with Publish.Add: %d" x)

    simpleEvent.Trigger(5)

    let eventForDelegateType = new Event<EventHandler, EventArgs>()

    eventForDelegateType.Publish.AddHandler(
        EventHandler(fun _ _ -> printfn "this is handler was added with Publish.AddHandler"))

    eventForDelegateType.Trigger(null, EventArgs.Empty)

module DatabaseAccess = ()

module OData = ()


#if COMPILED

module BoilerPlateForForm =
    [<System.STAThread>]
    do ()

#endif