open TestUtils
open Fsm.Nfa
open Fsm.Regexp
open Fsm.Utils

module A = Alcotest 

let student_test1 _ = 
  assert_true true
;;

let suite =
  let open Alcotest in
  [
    ( "student_sample_test"
    , [ test_case "student_test1" `Quick student_test1
    ] )
  ]

let () =
  Alcotest.run "student" suite
