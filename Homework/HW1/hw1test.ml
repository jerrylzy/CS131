let my_subset_test0 = subset [] []
let my_subset_test1 = subset [3;1;3] [1;3;1]
let my_subset_test2 = not (subset [1] [])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1] [1;1;1;1;1;1]
let my_equal_sets_test1 = not (equal_sets [] [1])

let my_set_union_test0 = equal_sets (set_union [1;2;3] []) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;2;3] [3;1;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1;1;1;1;1] []) [1]
let my_set_union_test3 = equal_sets (set_union [] [1;1;1;1;1]) [1]

let my_set_intersection_test0 =
  equal_sets (set_intersection [1;2;3] []) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [1;2;3] [3;1;3]) [1;3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [3;1;2;4] [1;2;3;4]) [4;3;2;1]
let my_set_intersection_test3 =
  equal_sets (set_intersection [1;1;1;1;1] [1; 1]) [1]
let my_set_intersection_test4 =
  equal_sets (set_intersection [1;1;1;1;1] [1]) [1]
let my_set_intersection_test5 =
  equal_sets (set_intersection [1] [1;1;1;1;1]) [1]
let my_set_intersection_test6 =
  equal_sets (set_intersection ["a"] ["a"; "a"; "a"; "a"; "a"]) ["a"]


let my_set_diff_test0 = equal_sets (set_diff [1;2;3;4;1] [1;4;3]) [2]
let my_set_diff_test1 = equal_sets (set_diff [] []) []

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> sqrt x) 10000. = 1.

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> -x) 2 (-1) = -1

let my_while_away_test0 = 
  equal_sets (while_away ((+) 3) ((>) 10) 0) [0; 3; 6; 9]
let my_while_away_test1 = 
  equal_sets (while_away ((+) 3) ((<) 10) 0) []

let my_rle_decode_test0 = 
  equal_sets (rle_decode [2,0; 1,6]) [0; 0; 6]
let my_rle_decode_test1 = 
  equal_sets (rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"]) ["w"; "w"; "w"; "x"; "z"; "z"]
let my_rle_decode_test2 =
  equal_sets (rle_decode [0, 2.0; 2, 0.0]) [0.0; 0.0]

type nonterminals =
  | Expr | Eggert | Grade | Retake

let rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Eggert];
    Expr, [N Expr; N Eggert; N Expr];
    Eggert, [N Eggert];
    Eggert, [N Expr; T"Workload"; N Expr];
    Eggert, [N Expr; T"Angel"];
    Grade, [T"Bad Grade"];
    Grade, [T"No A+"];
    Retake, []]

let grammar = Expr, rules

let my_filter_blind_alleys_test0 = filter_blind_alleys grammar 
    = (Expr, [Grade, [T"Bad Grade"];
       Grade, [T"No A+"];
       Retake, []])

let my_filter_blind_alleys_test1 =
  filter_blind_alleys (Expr,
      [Expr, [T"("; N Expr; T")"];
       Expr, [N Eggert];
       Expr, [N Expr; N Eggert; N Expr];
       Eggert, [N Eggert];
       Eggert, [N Expr; T"Workload"; N Expr];
       Eggert, [N Expr; T"Angel"];
       Eggert, [N Grade];
       Grade, [T"Bad Grade"];
       Grade, [T"No A+"];
       Retake, [N Expr];
       Retake, [T"C-"; N Retake];
       Retake, [N Eggert]])
  = (Expr,
      [Expr, [T"("; N Expr; T")"];
       Expr, [N Eggert];
       Expr, [N Expr; N Eggert; N Expr];
       Eggert, [N Eggert];
       Eggert, [N Expr; T"Workload"; N Expr];
       Eggert, [N Expr; T"Angel"];
       Eggert, [N Grade];
       Grade, [T"Bad Grade"];
       Grade, [T"No A+"];
       Retake, [N Expr];
       Retake, [T"C-"; N Retake];
       Retake, [N Eggert]])
