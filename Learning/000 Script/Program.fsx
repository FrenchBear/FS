// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let rec fact i =
  match i with
  | 0 -> 1
  | n -> n*fact (n-1)

let f10 = fact 10
printfn "10! = %d" f10
