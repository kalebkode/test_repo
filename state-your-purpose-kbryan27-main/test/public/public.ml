open Fsm.Nfa
open Fsm.Regexp
open Fsm.Utils
open TestUtils

let test_nfa_accept () =
  let m1 = (['a'; 'b'], [0; 1], 0, [1],  [{input=Some 'a'; states=(0, 1)}]) in
  assert_nfa_deny m1 "" ;
  assert_nfa_accept m1 "a" ;
  assert_nfa_deny m1 "b" ;
  assert_nfa_deny m1 "ba" ;

  let m2 = (['a'; 'b'], [0; 1; 2], 0, [2], [{input=Some 'a'; states=(0, 1)}; {input=Some 'b'; states=(0, 2)}]) in
  assert_nfa_deny m2 "" ;
  assert_nfa_deny m2 "a" ;
  assert_nfa_accept m2 "b" ;
  assert_nfa_deny m2 "ba"

  
let test_nfa_to_dfa () =
  let m1 = (['a'; 'b'], 
            [0; 1; 2; 3], 
            0, 
            [1; 3], 
            [{input=Some 'a'; states=(0, 1)}; {input=Some 'a'; states=(0, 2)}; {input=Some 'b'; states=(2, 3)}]) in
  let m1' = nfa_to_dfa m1 in
  assert_dfa m1' str_of_int_lst_fsm;
  assert_nfa_deny m1' "" ;
  assert_nfa_accept m1' "a" ;
  assert_nfa_accept m1' "ab" ;
  assert_nfa_deny m1' "b" ;
  assert_nfa_deny m1' "ba" ;
  let m2 = (['a'; 'b'], [0; 1; 2], 0, [2], [{input=Some 'a'; states=(0, 1)}; {input=Some 'b'; states=(0, 2)}])
  in
  let m2' = nfa_to_dfa m2 in
  assert_dfa m2' str_of_int_lst_fsm;
  assert_nfa_deny m2' "" ;
  assert_nfa_deny m2' "a" ;
  assert_nfa_accept m2' "b" ;
  assert_nfa_deny m2' "ba"


let test_nfa_closure () =
  let m1 = (['a'], [0; 1], 0, [1], [{input=Some 'a'; states=(0, 1)}]) in
  assert_nfa_closure m1 [0] [0] ;
  assert_nfa_closure m1 [1] [1] ;

  let m2 = ([], [0; 1], 0, [1], [{input=None; states=(0, 1)}]) in
  assert_nfa_closure m2 [0] [0; 1] ;
  assert_nfa_closure m2 [1] [1] ;

  let m3 = (['a'; 'b'], [0; 1; 2], 0, [2], [{input=Some 'a'; states=(0, 1)}; {input=Some 'b'; states=(0, 2)}]) in
  assert_nfa_closure m3 [0] [0] ;
  assert_nfa_closure m3 [1] [1] ;
  assert_nfa_closure m3 [2] [2] ;

  let m4 = (['a'], [0; 1; 2], 0, [2], [{input=None; states=(0, 1)}; {input=None; states=(0, 2)}]) in
  assert_nfa_closure m4 [0] [0; 1; 2] ;
  assert_nfa_closure m4 [1] [1] ;
  assert_nfa_closure m4 [2] [2]


let test_nfa_move () =
  let m1 = (['a'], [0; 1], 0, [1], [{input=Some 'a'; states=(0, 1)}]) in
  assert_nfa_move m1 [0] (Some 'a') [1] ;
  assert_nfa_move m1 [1] (Some 'a') [] ;

  let m2 = (['a'], [0; 1], 0, [1], [{input=None; states=(0, 1)}]) in
  assert_nfa_move m2 [0] (Some 'a') [] ;
  assert_nfa_move m2 [1] (Some 'a') [] ;

  let m3 = (['a'; 'b'], [0; 1; 2], 0, [2], [{input=Some 'a'; states=(0, 1)}; {input=Some 'b'; states=(0, 2)}]) in
  assert_nfa_move m3 [0] (Some 'a') [1] ;
  assert_nfa_move m3 [1] (Some 'a') [] ;
  assert_nfa_move m3 [2] (Some 'a') [] ;
  assert_nfa_move m3 [0] (Some 'b') [2] ;
  assert_nfa_move m3 [1] (Some 'b') [] ;
  assert_nfa_move m3 [2] (Some 'b') [] ;

  let m4 = (['a'; 'b'], [0; 1; 2], 0, [2], [{input=None; states=(0, 1)}; {input=Some 'a'; states=(0, 2)}]) in
  assert_nfa_move m4 [0] (Some 'a') [2] ;
  assert_nfa_move m4 [1] (Some 'a') [] ;
  assert_nfa_move m4 [2] (Some 'a') [] ;
  assert_nfa_move m4 [0] (Some 'b') [] ;
  assert_nfa_move m4 [1] (Some 'b') [] ;
  assert_nfa_move m4 [2] (Some 'b') []


let test_nfa_new_states () =
  let m1 = 
    (
      ['a'; 'b'], 
      [0; 1; 2; 3; 4], 
      0, 
      [1;3], 
      [{input=Some 'a'; states=(0, 1)}; {input=Some 'a'; states=(0, 2)}; {input=Some 'b'; states=(2, 3)}; {input=None; states=(2, 4)}; {input=Some 'a'; states=(4, 4)}]
    ) in 

  assert_set_set_eq [[]; []] (new_states m1 []) ;
  assert_set_set_eq [[1; 2; 4]; []] (new_states m1 [0]) ;
  assert_set_set_eq [[4]; []] (new_states m1 [3; 4]) ;
  assert_set_set_eq [[1; 2; 4]; [3]] (new_states m1 [0; 2]) ;
  assert_set_set_eq [[1; 2; 4]; [3]] (new_states m1 [0; 1; 2; 3])


let test_nfa_new_trans () =
  let m1 = 
    (
      ['a'; 'b'],
      [0; 1; 2; 3; 4],
      0,
      [1; 3],
      [{input=Some 'a'; states=(0, 1)}; {input=Some 'a'; states=(0, 2)}; {input=Some 'b'; states=(2, 3)}; {input=None; states=(2, 4)}; {input=Some 'a'; states=(4, 4)}]
    ) in
  assert_trans_eq
    [{input=Some 'a'; states=([0], [1; 2; 4])}; {input=Some 'b'; states=([0], [])}]
    (new_trans m1 [0]) ;
  assert_trans_eq
    [{input=Some 'a'; states=([0; 2], [1; 2; 4])}; {input=Some 'b'; states=([0; 2], [3])}]
    (new_trans m1 [0; 2])

let test_nfa_new_finals () =
  let m1 =
    (
      ['a'; 'b'],
      [0; 1; 2; 3; 4],
      0,
      [1; 3],
      [{input=Some 'a'; states=(0, 1)}; {input=Some 'a'; states=(0, 2)}; {input=Some 'b'; states=(2, 3)}; {input=None; states=(2, 4)}; {input=Some 'a'; states=(4, 4)}]
    ) in
  assert_set_set_eq [] (new_finals m1 [0; 2]) ;
  assert_set_set_eq [[1]] (new_finals m1 [1]) ;
  assert_set_set_eq [[1; 3]] (new_finals m1 [1; 3])

let test_re_to_nfa () =
  let m1 = regexp_to_nfa (Char 'a') in
  assert_nfa_deny m1 "" ;
  assert_nfa_accept m1 "a" ;
  assert_nfa_deny m1 "b" ;
  assert_nfa_deny m1 "ba" ;
  
  let m2 = regexp_to_nfa (Union (Char 'a', Char 'b')) in
  assert_nfa_deny m2 "" ;
  assert_nfa_accept m2 "a" ;
  assert_nfa_accept m2 "b" ;
  assert_nfa_deny m2 "ba"

let test_str_to_nfa () =
  let m1 = regexp_to_nfa @@ string_to_regexp "ab" in
  assert_nfa_deny m1 "a" ;
  assert_nfa_deny m1 "b" ;
  assert_nfa_accept m1 "ab" ;
  assert_nfa_deny m1 "bb"

let suite =
  let open Alcotest in
  [
    ( "nfa_operations"
    , [ test_case "nfa_accept" `Quick test_nfa_accept
      ; test_case "nfa_closure" `Quick test_nfa_closure
      ; test_case "nfa_move" `Quick test_nfa_move
    ] )
  ; ( "nfa_conversion"
    , [ test_case "nfa_to_dfa" `Quick test_nfa_to_dfa
    ] )
  ; ( "nfa_to_dfa_helpers" 
    , [ test_case "nfa_new_states" `Quick test_nfa_new_states
      ; test_case "nfa_new_trans" `Quick test_nfa_new_trans
      ; test_case "nfa_new_finals" `Quick test_nfa_new_finals
    ] )
  ; ( "regexp_to_nfa"
    , [ test_case "re_to_nfa" `Quick test_re_to_nfa
      ; test_case "str_to_nfa" `Quick test_str_to_nfa
    ] )
  ]

let () =
  Alcotest.run "public" suite
