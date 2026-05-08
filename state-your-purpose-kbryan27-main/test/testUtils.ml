open Fsm.Nfa
(* open Fsm.Regexp *)
open Fsm.Utils

module A = Alcotest 
(* general helpers *)
let assert_true x = A.(check bool) "" true (true = x)

let assert_false x = A.(check bool) "" true (false = x)

let assert_pass () = A.(check bool) "" true (true = true)

let assert_fail () = A.(check bool) "" true (false = false)

let string_of_int_list = Fmt.to_to_string A.(int |> list |> pp)

let string_of_int_list_list = Fmt.to_to_string A.(int |> list |> list |> pp)

let assert_dfa m printer =
  let (_sigma, _qs, _q0, _fs, m_delta) = m in
  let nondet =
    List.fold_left
      (*(fun res (q, c, _) ->
        match c with*)
        (fun res s -> 
          match s.input with
          | None -> true
          | Some _ ->
            let others =
              List.filter (fun {input; states} -> (fst s.states) = (fst states) && s.input = input) m_delta
            in
            res || List.length others > 1)
    false m_delta
  in
  A.(check bool) (printer m ^ "\nis not a dfa") false nondet

(* add ppx_deriving to type, then remove this *)
let re_to_str = show_regexp_t
  
(* WRAPPERS *)

(************************)
(* (int list, char) NFA *)
(************************)

let str_of_int_lst_fsm = A.(show_nfa_t (int |> list |> pp) (pp char))

(***********************)
(*** (int, char) NFA ***)
(***********************)

let str_of_int_fsm = A.(show_nfa_t (pp int) (pp char))

(***************)
(*** ANY FSM ***)
(***************)

(***********************)
(*** EPSILON CLOSURE ***)
(***********************)

let wrapper_eclosure m expects results printer= 
  let rec test_eclosure_aux e r = match (e,r) with
  ([],[]) -> true
  |_::_,[] -> (failwith ("Don't think this should happen - null set generated a eclosure"))
  |[],x::_ -> (failwith ("Don't think this should happen - "^ string_of_int_list x ^ "did not generate an eclosure"))
  |x::xs,y::ys -> 
    let _ = (A.(check bool) ((printer m)^"\nThe eclosure of : " ^ string_of_int_list y ^ 
                " is " ^ string_of_int_list x ^ "' when it shouldn't be") true (x = y)) in
    test_eclosure_aux xs ys in
  test_eclosure_aux expects results

(************************)
(*** GENERALIZED MOVE ***)
(************************)

let wrapper_move m expects results printer= 
  let rec test_move_aux e r = match (e,r) with
  ([],[]) -> true
  |_::_,[] -> (failwith ("Don't think this should happen - null moved on a symbol"))
  |[],x::_ -> (failwith ("Don't think this should happen - "^ string_of_int_list x ^ "did not generate a move"))
  |x::xs,y::ys -> 
    let _ = (A.(check bool) ((printer m)^"\nMoving on : " ^ string_of_int_list y ^ 
                "was supposed to result in " ^ string_of_int_list x ^ "' but it did not, or did so on the wrong character") true (x = y)) in
    test_move_aux xs ys in
  test_move_aux expects results

(**********************)
(*** ACCEPT THE FSM ***)
(**********************)


(*********************)
(*** ACCEPT A LIST ***)
(*********************)

let wrapper_accept m expects results printer= 
  let rec test_accept_aux e r = match (e,r) with
  ([],[]) -> true
  |x::_,[] -> (failwith ("accept failed to accept " ^ x))
  |[],x::_ -> (failwith ("accepted " ^ x ^ " when it should not have"))
  |x::xs,y::ys -> 
    let _ = (A.(check bool) ((printer m)^"\neither does not accept '" ^ x ^ 
                "' or is accepting '" ^ y ^ "' when it shouldn't") true (x = y)) in
    test_accept_aux xs ys in
  test_accept_aux expects results

(*********************)
(*** ACCEPT SINGLE ***)
(*********************)

let wrapper_accept_sig m expects f s printer = 
    A.(check bool) ((printer m)^
              if expects then 
              "\ndid not accept '" ^ s ^ "' when it should have" 
              else "\naccepted '" ^ s ^ "' when it shouldn't have") true (expects = (f m s));;

let wrapper_accept_s f m s p = wrapper_accept_sig m true  f s p
let wrapper_deny_s   f m s p = wrapper_accept_sig m false f s p

(********************)
(*** CHECK IF DFA ***)
(********************)

let wrapper_assert_dfa m printer =
  let (_sigma, _qs, _q0, _fs, m_delta) = m in
  let nondet =
    List.fold_left
      (fun res s1 ->
        match s1.input with
        | None -> true
        | Some _ ->
          let others =
            List.filter (fun s2 -> (fst s2.states) = (fst s1.states) && s2.input = s1.input) m_delta
          in
          res || List.length others > 1)
    false m_delta
  in
  if nondet then (A.(check bool) ((printer m)^"\nis not a dfa") true (true = false))

let wrapper_assert_dfas ms p = let _ = List.map (fun x -> wrapper_assert_dfa x p) ms in ()

(* Helpers for clearly testing the accept function *)
let assert_nfa_accept nfa input =
  if not @@ accept nfa input then
    A.(fail
    @@ Printf.sprintf "NFA should have accept string '%s', but did not" input)

let assert_nfa_deny nfa input =
  if accept nfa input then
    A.(fail
    @@ Printf.sprintf "NFA should not have accepted string '%s', but did" input)

let assert_nfa_closure nfa ss es =
  let es = List.sort compare es in
  let rcv = List.sort compare @@ e_closure nfa ss in
  if not (es = rcv) then
    A.(fail
    @@ Printf.sprintf "Closure failure: Expected %s, received %s"
         (string_of_int_list es) (string_of_int_list rcv))

let assert_nfa_move nfa ss mc es =
  let es = List.sort compare es in
  let rcv = List.sort compare @@ move nfa ss mc in
  if not (es = rcv) then
    A.(fail
    @@ Printf.sprintf "Move failure: Expected %s, received %s"
         (string_of_int_list es) (string_of_int_list rcv))

let assert_set_set_eq lst1 lst2 =
  let es l = List.sort_uniq compare (List.map (List.sort compare) l) in
  A.(check bool) "" true ((es lst1) = (es lst2))

let assert_trans_eq lst1 lst2 =
  let es l =
    List.sort_uniq compare
      (List.map
         (fun {input=t; states=(l1, l2)} -> {input=t; states=(List.sort compare l1, List.sort compare l2)})
         l)
  in
  A.(check bool) "" true ((es lst1) = (es lst2))

let assert_set_eq lst1 lst2 =
  let es = List.sort_uniq compare in
  A.(check bool) "" true ((es lst1) = (es lst2))
