(* IMPORTANT: YOU MAY NOT MODIFY THIS FILE!
 * OUR TESTS USE THE ORIGINAL VERSION.
 * YOUR CODE WILL NOT COMPILE IF YOU CHANGE THIS FILE. *)

(* This is the regexp function you must implement *)

val regexp_to_nfa : Utils.regexp_t -> (int, char) Utils.nfa_t

(* These are types for functions that we wrote *)

val string_to_regexp : string -> Utils.regexp_t

val string_to_nfa : string -> (int, char) Utils.nfa_t