// 29 State machine
// Learning F# II, p.26
//
// 2024-08-11   PV

//Here’s an example using the shopping cart state machine:
type ActiveCartData = { UnpaidItems: string list }

type PaidCartData =
    { PaidItems: string list
      Payment: float }

type ShoppingCart =
    | EmptyCart // no data
    | ActiveCart of ActiveCartData
    | PaidCart of PaidCartData

let addItem cart item =
    match cart with
    | EmptyCart ->
        // create a new active cart with one item
        ActiveCart { UnpaidItems = [ item ] }
    | ActiveCart { UnpaidItems = existingItems } ->
        // create a new ActiveCart with the item added
        ActiveCart { UnpaidItems = item :: existingItems }
    | PaidCart _ ->
        // ignore
        cart

let makePayment cart payment =
    match cart with
    | EmptyCart ->
        // ignore
        cart
    | ActiveCart { UnpaidItems = existingItems } ->
        // create a new PaidCart with the payment
        PaidCart
            { PaidItems = existingItems
              Payment = payment }
    | PaidCart _ ->
        // ignore
        cart


// Designing event handling functions
// Guideline: Event handling functions should always accept and return the entire state machine

// While the original makePayment takes a card and results in a cart (val makePayment : ShoppingCart -> float -> ShoppingCart),
// this new function (val makePayment2 :  ActiveCartData -> float -> PaidCartData) is bad:
let makePayment2 activeCart payment =
    let {UnpaidItems=existingItems} = activeCart
    {PaidItems = existingItems; Payment=payment}
// Then how would you handle the same event when the cart was in a different state, such as empty or paid? Someone has
// to handle the event for all three possible states somewhere, and it is much better to encapsulate this business logic
// inside the function than to be at the mercy of the caller.




// ----------------------------------------------------------------------
// Using explicit states to replace boolean flags

// In the Contact example from an earlier post we had a flag that was used to indicate whether a customer had verified
// their email address. The type looked like this:
type EmailContactInfo =
    {
    EmailAddress: string;       // Actually EmailAddress.T
    IsEmailVerified: bool;
    }


module EmailContactInfo =
    open System
    // placeholder
    type EmailAddress = string
    // UnverifiedData = just the email (an alias)
    type UnverifiedData = EmailAddress
    // VerifiedData = email plus the time it was verified
    type VerifiedData = EmailAddress * DateTime
    // set of states
    type T =
        | UnverifiedState of UnverifiedData
        | VerifiedState of VerifiedData


    // Now let’s handle the construction of a new state machine, and then the events.
    // •	Construction always results in an unverified email, so that is easy.
    // •	There is only one event that transitions from one state to another: the “verified” event.

    let create email = UnverifiedState email       // unverified on creation
    
    // handle the "verified" event
    let verified emailContactInfo dateVerified =
        match emailContactInfo with
        | UnverifiedState email ->
            // construct a new info in the verified state
            VerifiedState (email, dateVerified)
        | VerifiedState _ ->
            emailContactInfo       // ignore


    // Finally, we can write the two utility functions sendVerificationEmail and sendPasswordReset.

    let sendVerificationEmail emailContactInfo =
        match emailContactInfo with
        | UnverifiedState email -> printfn "sending email"
        | VerifiedState _ -> ()   // do nothing

    let sendPasswordReset emailContactInfo =
        match emailContactInfo with
        | UnverifiedState email ->
            // ignore
            ()
        | VerifiedState _ ->
            // ignore
            printfn "sending password reset"

