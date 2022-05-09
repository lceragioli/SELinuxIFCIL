module SS = Set.Make (String)

module OrderList (Ord : Stdlib__set.OrderedType) = struct
  include List
  type t = Ord.t list
  let compare = List.compare Ord.compare
end;;

module OrderPair (Ord1 : Stdlib__set.OrderedType) (Ord2 : Stdlib__set.OrderedType) = struct
  type t = Ord1.t * Ord2.t
  let compare (a, b) (c, d) = 
    if Ord1.compare a c == 0 then Ord2.compare b d else Ord1.compare a c
end;;

module StringList = OrderList (String)
module SLS = Set.Make (StringList)

module StSL = OrderPair (String) (StringList)
module StSLS = Set.Make (StSL)

module SM = Map.Make (String)
module SLM = Map.Make (StringList)

let ( << ) f g x = f (g x)

let rec listminus list list' =
  if list' = [] then Some list
  else if list = [] then None
  else if List.hd list = List.hd list' then
    listminus (List.tl list) (List.tl list')
  else None

let replace input output = Str.global_replace (Str.regexp_string input) output

let remove_space = replace " " ""

let slist_to_ss ls = List.fold_left (fun ss s -> SS.add s ss) SS.empty ls

let tl_flatten ls =
  let rec go acc = function
    | [] -> List.rev acc
    | l :: r -> go (List.rev_append l acc) r
  in
  go [] ls

let last_and_list lst =
  let rv = List.rev lst in
  (List.hd rv, List.rev (List.tl rv))

let rec list_find_map f ls =
  match ls with
  | [] -> None
  | a :: als -> (
      match f a with None -> list_find_map f als | Some b -> Some b)

let rec prefix_list ls =
  match ls with
  | [] -> [ [] ]
  | [ a ] -> [ [ a ] ]
  | a :: als ->
      let ls' = prefix_list als in
      [ a ] :: List.map (List.cons a) ls'

let id x = x

let breake ls =
  List.filter
    (fun (l1, l2) -> l1 != [] && l2 != [])
    (List.fold_left
       (fun lps l ->
         match List.hd lps with
         | l1, l2 ->
             (l1 @ [ l ], []) :: List.map (fun (l1, l2) -> (l1, l2 @ [ l ])) lps)
       [ ([], []) ] ls)

let times ls ls' =
  List.flatten
    (List.map
       (fun (l1, l2) -> List.map (fun (l1', l2') -> ((l1, l1'), (l2, l2'))) ls')
       ls)

let times_list ls ls' =
  List.flatten (List.map (fun l1 -> List.map (fun l2 -> (l1, l2)) ls') ls)

let remove_duplicate meets =
  List.fold_left
    (fun ks k -> if List.exists (fun k' -> k' = k) ks then ks else k :: ks)
    [] meets

let make_pairs ls ls' =
  let brkls = breake ls and brkls' = breake ls' in
  times brkls brkls'

let rec list_start_with ls ls' =
  match (ls, ls') with
  | ls, [] -> true
  | l :: ls, l' :: ls' -> l = l' && list_start_with ls ls'
  | _ -> false
