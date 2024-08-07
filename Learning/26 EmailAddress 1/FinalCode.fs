module FinalCode

module EmailAddress =
    type T = EmailAddress of string

    // create with continuation
    let createWithCont success failure (s: string) =
        if System.Text.RegularExpressions.Regex.IsMatch(s, @"^\S+@\S+\.\S+$") then
            success (EmailAddress s)
        else
            failure "Email address must contain an @ sign"

    // create directly
    let create s =
        let success e = Some e
        let failure _ = None
        createWithCont success failure s

    // unwrap with continuation
    let apply f (EmailAddress e) = f e

    // unwrap directly
    let value e = apply id e


module ZipCode =
    type T = ZipCode of string

    // create with continuation
    let createWithCont success failure (s: string) =
        if System.Text.RegularExpressions.Regex.IsMatch(s, @"^\d{5}$") then
            success (ZipCode s)
        else
            failure "Zip code must be 5 digits"

    // create directly
    let create s =
        let success e = Some e
        let failure _ = None
        createWithCont success failure s

    // unwrap with continuation
    let apply f (ZipCode e) = f e

    // unwrap directly
    let value e = apply id e


module StateCode =
    type T = StateCode of string

    // create with continuation
    let createWithCont success failure (s: string) =
        let s' = s.ToUpper()
        let stateCodes = [ "AZ"; "CA"; "NY" ] // etc

        if stateCodes |> List.exists ((=) s') then
            success (StateCode s')
        else
            failure "State is not in list"

    // create directly
    let create s =
        let success e = Some e
        let failure _ = None
        createWithCont success failure s
        //createWithCont (fun e -> Some e) (fun _ -> None) s        // Same thing with lambdas

    // unwrap with continuation
    let apply f (StateCode e) = f e
    
    // unwrap directly
    let value e = apply id e


type PersonalName =
    { FirstName: string
      MiddleInitial: string option
      LastName: string }

type EmailContactInfo =
    { EmailAddress: EmailAddress.T
      IsEmailVerified: bool }

type PostalAddress =
    { Address1: string
      Address2: string
      City: string
      State: StateCode.T
      Zip: ZipCode.T }

type PostalContactInfo =
    { Address: PostalAddress
      IsAddressValid: bool }

type Contact =
    { Name: PersonalName
      EmailContactInfo: EmailContactInfo
      PostalContactInfo: PostalContactInfo }
