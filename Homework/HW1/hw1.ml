type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec contains e s = match s with
| [] -> false
| h::t -> if h = e then true
		  else (contains e t)

let rec subset a b = match a with
| [] -> true
| h::t -> (contains h b) && (subset t b)

let equal_sets a b = (subset a b) && (subset b a)

let rec set_union a b = match a with
| [] -> b
| h::t -> if (contains h b) then (set_union t b)
		  else h::(set_union t b)

let rec set_intersection a b = match a with
| [] -> []
| h::t -> if (contains h b) then h::(set_intersection t b)
		  else (set_intersection t b)

let rec set_diff a b = match a with
| [] -> []
| h::t -> if (contains h b) then (set_diff t b)
		  else h::(set_diff t b)

let rec computed_fixed_point eq f x = 
	let value = f x in
	if (eq value x) then x
	else (computed_fixed_point eq f value)

let rec run_p_times f p x =
	if p = 1 then (f x)
	else (f (run_p_times f (p - 1) x))

let rec computed_periodic_point eq f p x = match p with
| 0 -> x
| 1 -> (computed_fixed_point eq f x)
| _ -> if (eq (run_p_times f p x) x) then x
	   else (computed_periodic_point eq f p (f x))

let rec while_away s p x =
	if not (p x) then []
	else x::(while_away s p (s x))

let rec append_word (x, y) = match x with
| 0 -> []
| _ -> y::(append_word ((x - 1), y))

let rec rle_decode lp = match lp with
| [] -> []
| h::t -> set_union (append_word h) (rle_decode t)

let is_terminal expr safe_lists = match expr with
| N e -> (contains e safe_lists)
| T _ -> true

(* rule = [N Exp], safe_lists = [Expr; Expr; ...] *)
(* This function will walk through all symbols and see if they are safe *)
let rec is_safe rules safe_lists = match rules with
| [] -> true
| h::t -> 	if not (is_terminal h safe_lists) then false
			else (is_safe t safe_lists)

(* safe_lists = Expr; Expr; ...; ], orig_rules = [ (Expr, [ ... ])* ] *)
let rec add_safe_rules safe_lists orig_rules = match orig_rules with
| [] -> safe_lists
| h::t ->	if (is_safe (snd h) safe_lists) && not (contains (fst h) safe_lists)
			then (add_safe_rules ((fst h)::safe_lists) t)
			else (add_safe_rules safe_lists t)

(* This wrapper helps to deal with the fixed point function *)
(* The return type and the parameter type have to match *)
let rec add_safe_rules_wrapper (safe_lists, orig_rules) =
	(add_safe_rules safe_lists orig_rules), orig_rules

(* Also created for the fixed point function to match types *)
let rec equal_sets_wrapper (x1, g1) (x2, g2) = equal_sets x1 x2

(* orig_rules = [Expr, [Expr; ...; ]] *)
let rec filter safe_rules orig_rules = match orig_rules with
| [] -> []
| h::t -> 	if (is_safe (snd h) safe_rules) then h::(filter safe_rules t)
			else (filter safe_rules t)

(* First find out all safe rules, *)
(* and delete rules that are not completely consisted of the safe ones *)
let filter_blind_alleys g = 
	let rules = (snd g) in
	(fst g), (filter (fst (computed_fixed_point 
		equal_sets_wrapper add_safe_rules_wrapper ([], rules))) rules)

