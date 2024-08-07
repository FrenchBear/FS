// Signature file EmailAddress.fsi

module EmailAddress

// encapsulated type
type T

// wrap
val create : string -> T option

// unwrap
val value : T -> string
