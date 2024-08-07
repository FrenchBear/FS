// 26 EmailAddress 2
// Learning F# II, Variations over email address, part 2
// Testing signature (.fsi) and implementation (.fs) files
//
// 2024-08-07   PV

module EmailAddressClient =

    // code works when using the published functions
    let address1 = EmailAddress.create "x@example.com"
    let address2 = EmailAddress.create "example.com"

    // code that uses the internals of the type fails to compile
    //let address3 = T.EmailAddress "bad email"

    // This doesn't work either
    //let address4 = EmailAddress.EmailAddress "Hello world"
