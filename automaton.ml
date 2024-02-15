(* ------------------------ Definitions ------------------------ *)

module State =
  struct
    type t = int
    let compare = compare
    let hash x = x
    let equal x y = x = y
    let mk =
      let id = ref (-1) in
      fun () -> incr id; !id
end

module StateSet = Set.Make(State)

type automata = {
  name   : string;
  states : StateSet.t;
  start  : StateSet.t;
  final  : StateSet.t;
  trans  : ((State.t * char option), StateSet.t) Hashtbl.t;
  (* trans_e : (State.t, StateSet.t) Hashtbl.t; *)
  cache   : ((StateSet.t * char), StateSet.t) Hashtbl.t;
}

type re =
    Epsilon
  | Char of char
  | Alt of re * re
  | Concat of re * re
  | Star of re

(* ---------------------------- Utils ---------------------------- *)

let get_trans h q c =
  try
    Hashtbl.find h (q,c)
  with
  Not_found -> StateSet.empty

let mk_trans h q c l =
  Hashtbl.add h (q, c) (StateSet.of_list l)


(* ---------------------------- Next ----------------------------- *)

let next_epsilon auto states =
  let rec loop old =
    let res =
      StateSet.fold (fun q acc ->
        let t = get_trans auto.trans q None in
        StateSet.union t acc
      ) old old
    in
    if StateSet.equal res old then res
    else loop res
  in
  let states = loop states in
  StateSet.fold (fun q acc ->
    let t = get_trans auto.trans q (Some c) in
    StateSet.union t acc  
  ) states StateSet.empty

let next (auto: automata) (states: StateSet.t) (l: char) =
  let key = (states, l) in
  try
    Hashtbl.find auto.cache key
  with
    Not_found ->
    let res =
      next_epsilon auto states
  in
  Hashtbl.add auto.cache key res; res

let accept auto s =
  let rec sub_test states i =
    if StateSet.is_empty states then false
    else if i >= String.length s then
      not (StateSet.disjoint states auto.final)
    else
      sub_test (next auto states s.[i]) (i+1)
  in sub_test auto.start 0
  
let naive_accept auto s =
  let rec loop q i =
    if i >= String.length s then StateSet.mem q auto.final
    else
      let c = s.[i] in
      try
        let states = StateSet.union
          (Hashtbl.find auto.trans (q, Some c))
          (Hashtbl.find auto.trans (q, None)) in (* wrong because shouldn't do i+1 if reading epsilon transition*)
        StateSet.exists (fun q' -> loop q' (i+1)) states
      with
        Not_found -> false
    in
  StateSet.exists (fun q -> loop q 0) auto.start

let union auto_list s =
  List.exists (fun a -> accept a s) auto_list

let inter auto_list s =
  List.for_all (fun a -> accept a s) auto_list
  

(* --------------------------- Thompson -------------------------- *)

let rec pp fmt re =
  let open Format in
  match re with
    | Epsilon -> fprintf fmt "ϵ"
    | Char c  -> fprintf fmt "%C" c
    | Alt (re1, re2) ->
      fprintf fmt "(%a|%a)" pp re1 pp re2
    | Concat (re1, re2) ->
      fprintf fmt "(%a%a)" pp re1 pp re2
    | Star re ->
      fprintf fmt "(%a)*" pp re

let thompson re =
  let states = ref StateSet.empty in
  let mk_state () =
    let q = State.mk () in
      states := StateSet.add q !states;
      q
  in
  let trans = Hashtbl.create 16 in
  let rec loop re =
    let q_in = mk_state () in
    let q_out = mk_state () in
    let () =
      match re with
          Epsilon -> mk_trans trans q_in None [q_out]
        | Char c -> mk_trans trans q_in (Some c) [q_out]
        | Alt (re1, re2) ->
          let q_in1, q_out1 = loop re1 in
          let q_in2, q_out2 = loop re2 in
          mk_trans trans q_in None [q_in1; q_in2];
          mk_trans trans q_out1 None [q_out];
          mk_trans trans q_out2 None [q_out];
        | Concat (re1, re2) ->
          let q_in1, q_out1 = loop re1 in
          let q_in2, q_out2 = loop re2 in
          mk_trans trans q_in None [q_in1];
          mk_trans trans q_out1 None [q_in2];
          mk_trans trans q_out2 None [q_out];
        | Star re' ->
          let q_in0, q_out0 = loop re' in
          mk_trans trans q_in None [q_in0];
          mk_trans trans q_in0 None [q_out0];
          mk_trans trans q_out0 None [q_in0];
          mk_trans trans q_out0 None [q_out];
        in q_in, q_out
      in
      let q_in, q_out = loop re in ()
      (* {
        name = Format.asc;
        states = 
        start = 
        final = 
        trans = 
      } *)

(* --------------------------- Main -------------------------- *)

let auto_ex = 
  let q0 = State.mk () in
  let q1 = State.mk () in
  let trans = Hashtbl.create 16 in
  let states = StateSet.of_list [q0; q1] in
  let start = StateSet.singleton q0 in
  let final = StateSet.singleton q1 in
  mk_trans trans q0 (Some 'a') [q0];
  mk_trans trans q0 (Some 'b') [q1];
  { name="ex"; states; start; final; trans; cache=Hashtbl.create 16 }

let main () =
  let test = [
    (auto_ex, "aaaab");
    (auto_ex, "aaaac");
  ] in
  List.iter (fun (auto, w) ->
    Format.printf "%s: %s -> %b\n"
    auto.name w (naive_accept auto w)) test;;

main;;