type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Nested function, only convert_grammar can use find_rules *)
let convert_grammar gram1 =
	let rec find_rules nt = function
		| [] -> []
		| (expr, rules_list)::t ->
			if nt = expr then
				rules_list::(find_rules nt t)
			else
				find_rules nt t
	in (fst gram1, fun nt -> find_rules nt (snd gram1))

(*
	Acceptor: derivation d, fragment frag
*)

(* 
	matcher: Fragment list -> if accepted, return what the acceptor returns
	Otherwise, go to the next prefix and corresponding suffix.
	If there's no match, return None.
*)

let parse_prefix gram =
	let start_sym = fst gram
	and prod_func = snd gram
	in

	(* Array appendation. This is faster than @ op since it does not copy every element *)
	let rec append_array rhs = function
		| [] -> rhs
		| h::t -> h::(append_array rhs t)
	in

	(* syntax sugar: the pattern being matched is the list of rules for the current nonterminal*)
	let rec match_nonterminal prod_func nt = function
		(* Rule is exhausted and we haven't find an accepted match, return None *)
		| [] -> (fun accept derivation frag -> None)
		| rules_head::rules_tail ->
		(fun accept derivation frag ->
			let head_matcher = match_terminal prod_func rules_head
			and tail_matcher = match_nonterminal prod_func nt rules_tail
			in
			(* If we can find a match for rules_head, then just return whatever the acceptor returns *)
			(* If not, we will abandon rules_head and continue to find the next acceptable match *)
			(* This implements the idea of alternative list in the spec *)
			let ormatch = head_matcher accept (append_array [(nt, rules_head)] derivation) frag
			in match ormatch with
				| None -> tail_matcher accept derivation frag
				| _ -> ormatch)

	(* again syntax sugar, the pattern being matched is the list of rules of the current nonterminal *)
	and match_terminal prod_func = function
		(* no further rule to examine, return whatever acceptor returns *)
		| [] -> (fun accept derivation frag -> accept derivation frag)
		(* Found a terminal symbol *)
		| (T t)::rules_tail ->
			(fun accept derivation -> function
				(* Nothing in fragment but the rule needs more symbols, reject *)
				| [] -> None
				| frag_head::frag_tail ->
					(* Local variables make code easier to read *)
					let matcher = match_terminal prod_func rules_tail
					in if frag_head = t then (* Matches, ask the matcher to match the rest of frag *)
						matcher accept derivation frag_tail
					(* The terminal symbol in the rule does not match the one in fragment *)
					else
						None
			)
		(* Found a nonterminal symbol, ask the nonterminal matcher to match the found nonterminal *)
		| (N nt)::rules_tail ->
			(fun accept derivation frag ->
				let tail_matcher = match_nonterminal prod_func nt (prod_func nt)
				and new_acceptor = match_terminal prod_func rules_tail accept
				in tail_matcher new_acceptor derivation frag)
	in
	(* Just to match Eggert's requirement of function signature :) *)
	fun accept frag -> match_nonterminal prod_func start_sym (prod_func start_sym) accept [] frag
