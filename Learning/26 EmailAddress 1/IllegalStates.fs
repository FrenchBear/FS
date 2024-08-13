// Learning F# II 

module IllegalStates

open FinalCode


// Say that we have the following simple business rule: “A contact must have an email or a postal address
type ContactInfo =
    | EmailOnly of EmailContactInfo
    | PostOnly of PostalContactInfo
    | EmailAndPost of EmailContactInfo * PostalContactInfo

type Contact = {
    Name: PersonalName;
    ContactInfo: ContactInfo;
    }


let contactFromEmail name emailStr =
    let emailOpt = EmailAddress.create emailStr

    // handle cases when email is valid or invalid
    match emailOpt with
    | Some email ->
        let emailContactInfo = {EmailContactInfo.EmailAddress=email; IsEmailVerified=false}
        let contactInfo = EmailOnly emailContactInfo
        Some {Contact.Name=name; ContactInfo=contactInfo}
    | None -> None


// Updating a ContactInfo is complicated...
let updatePostalAddress contact newPostalAddress =
    let {Name=name; ContactInfo=contactInfo} = contact

    let newContactInfo =
        match contactInfo with
        | EmailOnly email ->
            EmailAndPost (email,newPostalAddress)
        | PostOnly _ -> // ignore existing address
            PostOnly newPostalAddress
        | EmailAndPost (email,_) -> // ignore existing address
            EmailAndPost (email,newPostalAddress)
    
    // make a new contact
    {Name=name; ContactInfo=newContactInfo}


let Test() =
    printfn "\n======================================================\nIllegalStates"

    let name = {PersonalName.FirstName = "A"; MiddleInitial=None; LastName="Smith"}
    let contactOpt = contactFromEmail name "abc@example.com"
    printfn "contactOpt: %0A" contactOpt

    let contact = contactOpt.Value   // using option.Value is not a good practice, should use a match...
    let newPostalAddress =
        let state = StateCode.create "CA"
        let zip = ZipCode.create "97210"
        {
            Address =
                {
                Address1= "123 Main";
                Address2="";
                City="Beverly Hills";
                State=state.Value;
                Zip=zip.Value;
                };
            IsAddressValid=false
        }
    let newContact = updatePostalAddress contact newPostalAddress
    printfn "newContact: %0A" newContact

