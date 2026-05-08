open List
open Utils

(*********)
(* Types *)
(*********)

(* 
  from utils.ml, for your reference

  type ('q, 's) transition = {
    input: 's option; 
    states: 'q * 'q;
  }

  (** NFA type *)
  type ('q,'s) nfa_t = ('s list * 'q list * 'q * 'q list * ('q, 's) transition list)
*)

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa : ('q, 's) nfa_t) (qs : 'q list) (s : 's option) : 'q list =
  (*deconstruct nfa to call on its parameters*)
  let sigma, states, start, finals, delta = nfa in
  (*check that s doesnt equal None, and isnt in the set of states*)
  match s with
  | Some sym when not (Utils.elem sym sigma) ->
      (*if nonexistant transition, return empty*)
      []
  | _ ->
      (*helper to check each state given in qs*)
      let rec help_states states_left dest_states =
        match states_left with
        | [] -> dest_states
        | curr :: tail ->
            (*define newly found desitantion states*)
            let new_dest_states =
              (*fold over delta list*)
              List.fold_left
                (*fold function takes in accumulator(dest list) and a transition*)
                (fun acc (transition : ('q, 's) transition) ->
                  (*deconstruct transition to use values*)
                  let trans_source, trans_dest = transition.states in
                  let trans_sym = transition.input in

                  (*if source equals current state (x) and symbols(s) match*)
                  if trans_source = curr && trans_sym = s then
                    (*insert the destination into acc*)
                    Utils.insert trans_dest acc
                  else
                    (*if no matches, return acc*)
                    acc)
                dest_states (*destination states for fold function*)
                delta (*fold over every transition in the nfa*)
            in
            (*call again on remaining states, with updated accumulator of destination states*)
            help_states tail new_dest_states
      in
      (*start helper on given qs's, and an empty list*)
      help_states qs []

let e_closure (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list =
  (*deconstruct nfa to call on its parameters*)
  let sigma, states, start, finals, delta = nfa in
  (*helper to check each state given in qs*)
  let rec help_epsilon states_left dest_states =
    match states_left with
    (*if end of list, return dest states collected*)
    | [] -> dest_states
    | curr :: tail ->
        (*define newly found desitantion states*)
        let new_dest_states =
          (*fold over delta list*)
          List.fold_left
            (*fold function takes in accumulator(dest list) and a transition*)
            (fun acc (transition : ('q, 's) transition) ->
              (*deconstruct transition to use values*)
              let trans_source, trans_dest = transition.states in
              let trans_sym = transition.input in

              (*if source equals current state (x) and symbols(None for epsilon) match*)
              if trans_source = curr && trans_sym = None then
                (*insert the destination into acc*)
                Utils.insert trans_dest acc
              else
                (*if no matches, return acc*)
                acc)
            [] (*destination states for fold function*)
            delta (*fold over every transition in the nfa*)
        in
        (*call again on remaining states, with updated accumulator of destination states*)
        (*calls with epsilon destination non duplicates because a chain of epsilon trans are valid*)
        help_epsilon
          (Utils.insert_all new_dest_states tail)
          (Utils.insert_all new_dest_states dest_states)
  in
  (*start helper on given qs's, and again on qs to be automatically included into output states*)
  help_epsilon qs qs

let accept (nfa : ('q, char) nfa_t) (s : string) : bool =
  (*deconstruct nfa to call on its parameters*)
  let sigma, states, start, finals, delta = nfa in
  (*explode string, (make it a list of transitions)*)
  let trans_list = Utils.explode s in

  (*helper to go through each char. curr states covers epsilon trans, chars left is rest of string*)
  let rec help_trans curr_states chars_left =
    match chars_left with
    (*if end of string, check potential states we could be in against potential final states*)
    (*if intersection is empty, return not true, if intersection matches, return not false*)
    | [] -> not (Utils.intersection curr_states finals = [])
    | curr :: tail ->
        (*get all states possible from current transition char*)
        (*use move and call on all potential states*)
        let move_states = move nfa curr_states (Some curr) in
        (*get all states possible from epsolon transitions, from those move states*)
        let e_states = e_closure nfa move_states in

        (*if move and epsilon return empty list, cannot accept because no states to go to(even tho we have more chars in string)*)
        if e_states = [] then false
        else
          (*otherwise call again with new list of current states, and the next chars in *)
          help_trans e_states tail
  in
  (*check start state for a list of epsilon trans*)
  let first_states = e_closure nfa [ start ] in
  (*start recusion on start state and its potential epsilon transitions*)
  help_trans first_states trans_list

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list list =
  (*deconstruct nfa to call on its parameters*)
  let sigma, states, start, finals, delta = nfa in
  (*must go through every symbol in sigma (List.map (fun) sigma)*)
  List.map
    (*function takes current char, gets accessible states and gets their e transitions like above*)
    (*this returns a list of all accesible states for the currently map'ed character*)
    (fun curr ->
      let move_states = move nfa qs (Some curr) in
      e_closure nfa move_states)
    sigma

let new_trans (nfa : ('q, 's) nfa_t) (qs : 'q list) :
    ('q list, 's) transition list =
  (*deconstruct nfa to call on its parameters*)
  let sigma, states, start, finals, delta = nfa in
  (*get destination states of every letter of alphabet*)
  let dest_list = new_states nfa qs in

  (*zip helper function to make a propper list of [(char, dest_states)]*)
  let rec help_zip symbols dests =
    match (symbols, dests) with
    (*if empty..., empty list*)
    | [], _ | _, [] -> []
    | curr_sym :: tail_sym, curr_dests :: tail_dests ->
        (curr_sym, curr_dests) :: help_zip tail_sym tail_dests
  in

  (*map over zipped tuples, create record type for all*)
  List.map
    (fun (curr, dests) -> { input = Some curr; states = (qs, dests) })
    (help_zip sigma dest_list)

let new_finals (nfa : ('q, 's) nfa_t) (qs : 'q list) : 'q list list =
  (*deconstruct nfa to call on its parameters*)
  let (sigma, states, start, finals, delta) = nfa in

  (*if any of states are in final, return double list qs, else empty list*)
    (*elem checks if a single state is in finals, exists goes through qs and returns a bool for passing*)
  if List.exists (fun curr_state -> Utils.elem curr_state finals) qs then
    [qs]
  else
    []

let rec nfa_to_dfa_step (nfa : ('q, 's) nfa_t) (dfa : ('q list, 's) nfa_t)
    (work : 'q list list) : ('q list, 's) nfa_t =
  (*deconstruct DFA*)
  let dfa_sigma, dfa_states, dfa_start, dfa_finals, dfa_delta = dfa in

  (*grab unvisited state*)
  match work with
  (*nothing left unvisited*)
  | [] -> dfa
  | curr_state :: tail_state ->
      (*get all transitions from current state with new_trans*)
      let trans_curr = new_trans nfa curr_state in

      (*getall destination states from current state with new_states*)
      let dest_states = new_states nfa curr_state in

      (*get final states, if final double list the current state/s*)
      let finals_curr = new_finals nfa curr_state in

      (*get destination states that are new (not in dfa_states)*)
      (*will be added to  worklist*)
      let new_states =
        List.filter (fun dest -> not (Utils.elem dest dfa_states)) dest_states
      in

      (*make updated dfa from new dest states, new trans statesm and new final states*)
      let updated_dfa =
        ( dfa_sigma,
          (*add new destination states with insert_all*)
          Utils.insert_all dest_states dfa_states,
          dfa_start,
          (*add new final states with insert_all*)
          Utils.insert_all finals_curr dfa_finals,
          (*add new transitions with insert_all*)
          Utils.insert_all trans_curr dfa_delta )
      in

      (*call on same nfa, updated dfa, and work+any new states*)
      nfa_to_dfa_step nfa updated_dfa (Utils.insert_all new_states tail_state)

let nfa_to_dfa (nfa : ('q, 's) nfa_t) : ('q list, 's) nfa_t =
  (*deconstruct nfa to call on its parameters*)
  let sigma, states, start, finals, delta = nfa in

  (*alwaus run eclosure on start state*)
  let dfa_start = e_closure nfa [start] in

  (*check if start state is end state*)
  let dfa_final = new_finals nfa dfa_start in

  (*dfa gets same sigma, eclosure start, eclosure start, potential known final state, and unknown transitions*)
  let dfa = (sigma, [ dfa_start ], dfa_start, dfa_final, []) in

  (*run step helper on nfa, partial dfa, and the first work states (start)*)
  nfa_to_dfa_step nfa dfa [dfa_start]
