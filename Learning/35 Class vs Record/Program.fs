// 35 Class vs Record
// Learning F#
// Simple comparison between classes and records
//
// 2024-08-30

open System.Collections.Generic

type PersonClass(Id: int, Name: string) =
    
    // let binding is allowed in classes for internal members
    let middleNames = new List<string>()

    static member createNew(p: {| Id: int; Name: string |}) = new PersonClass(p.Id, p.Name)

    member this.addMiddleName(mn: string) = middleNames.Add(mn)

    override this.ToString() =
        $"""PersonClass {{ Id = {Id}, Name = {Name}, Middle names = {System.String.Join("+", middleNames)} }}"""


let p = PersonClass.createNew {| Id = 12; Name = "Pierre" |}
p.addMiddleName "Bernard"
p.addMiddleName "René"

printfn "%0A" p // Since Person is a class, %0A calls ToString
printfn "%s" (p.ToString())



type PersonRecord =
    { Id: int
      Name: string
      MiddleNames: List<string>
    }

    // Augmentations of types do not support let bindings

    static member createNew id name = { PersonRecord.Id = id; Name = name; MiddleNames=new List<string>() }

    override this.ToString() =
        $"""PersonRecord {{ Id = {this.Id}, Name = {this.Name}, Middle names = {System.String.Join("+", this.MiddleNames)} }}"""


let p2 = PersonRecord.createNew 5 "Joe"
p2.MiddleNames.Add("Manfred")
p2.MiddleNames.Add("Gunther")
p2.MiddleNames.Add("Wilfried")
printfn "%0A" p2
printfn "%s" (p2.ToString())
