// 002 Module
// Learning F#, using a module
// Combining type and functions, type defined separately
//
// 2024-06-17   PV

namespace FS002

// declare the type outside the module
type PersonType = {First:string; Last:string}

// declare a module for functions that work on the type
module Person =

    // constructor
    let create first last =
        {First=first; Last=last}

    // method that works on the type
    let fullName {First=first; Last=last} =
        first + " " + last
