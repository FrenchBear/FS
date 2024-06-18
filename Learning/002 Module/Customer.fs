// 002 Module
// Learning F#, using a module
// Combining type and functions, type defined in the same module as functions
//
// 2024-06-17   PV

namespace FS002

module Customer =

    // Customer.T is the primary type for this module
    type T = {AccountId:int; Name:string}

    // constructor
    let create id name =
        {T.AccountId=id; T.Name=name}

    // method that works on the type
    let isValid {T.AccountId=id; } = id > 0
