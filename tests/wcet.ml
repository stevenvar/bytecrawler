external begin_loop : unit -> unit = "begin_loop"
external end_loop : unit -> unit = "end_loop"

external rand_bool : unit -> bool = "rand_bool"
external rand_int : unit -> int = "rand_bool"

type pin = PIN0 | PIN1 | PIN2 | PIN3 | PIN4 | PIN5 | PIN6 | PIN7 | PIN8 | PIN9 | PIN10
type level = LOW | HIGH
external digital_read : pin -> level = "rand_bool"
