// 34 CustomEquality CustomComparison
// Learning F#, Example showing how to implement specific equality and comparison
// https://www.compositional-it.com/news-blog/custom-equality-and-comparison-in-f/
//
// 2024-08-28   PV

open System

[<CustomEquality; CustomComparison>]
type Customer =
    { CustomerId: int
      Name: string
      Age: int
      Town: string }

    interface IEquatable<Customer> with
        member this.Equals other = other.CustomerId.Equals this.CustomerId

    override this.Equals other =
        match other with
        | :? Customer as p -> (this :> IEquatable<_>).Equals p
        | _ -> false

    override this.GetHashCode() = this.CustomerId.GetHashCode()


    interface IComparable<Customer> with
        member this.CompareTo other =
            this.CustomerId.CompareTo other.CustomerId

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Customer as p -> (this :> IComparable<_>).CompareTo p
            | _ -> -1



let c1 =
    { CustomerId = 1
      Name = "Pierre"
      Age = 58
      Town = "St Ismier" }

let c2 =
    { CustomerId = 2
      Name = "Claude"
      Age = 67
      Town = "St Ismier" }

let c3 =
    { CustomerId = 1
      Name = "Rex"
      Age = 3
      Town = "St Ismier" }

printfn "c1=c2: %A" (c1 = c2)       // False, Id ≠ Id 2
printfn "c1=c3: %A" (c1 = c3)       // True, Id 1 = Id 1
printfn "c1<c2: %A" (c1 < c2)       // True, Id 1 < Id 2
printfn "c1<c2: %A" (c1 < c3)       // False, Id 1 ≮ Id 1

