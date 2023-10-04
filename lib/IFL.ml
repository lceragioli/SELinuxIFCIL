(* open IFCILconfiguration *)
open IFCILsyntax
open Utils
module SS = Set.Make (String)

(* exception UndefinedMeet of string

exception NotUniqueMeet of string *)

exception UncorrectRefinement of string


let print_IFL_path (node, marrow, node') =
  let print_node = function
    | A_Type t
    | A_Attr t -> String.concat "." t
    | Any -> "*"
  in
  let marrowstring =
    match marrow with
    | m, LONGARROW -> " +[" ^ String.concat "," m ^ "]> "
    | m, SHORTARROW -> " [" ^ String.concat "," m ^ "]> "
  in
  " ( " ^ (print_node node) ^ ", " ^ marrowstring ^ ", "
  ^ (print_node node') ^ " ) "

let print_IFL_requirement_E r =
  match r with
  | E_MUST iflpath ->
      ".IFL-must "
      ^ List.fold_right (fun p s -> print_IFL_path p ^ s) iflpath ""
      ^ "\n"
  | E_MUSTNOT iflpath ->
      ".IFL-mustnot "
      ^ List.fold_right (fun p s -> print_IFL_path p ^ s) iflpath ""
      ^ "\n"
  | E_EVERYMUST (iflpath, iflpath') ->
      ".IFL-every "
      ^ List.fold_right (fun p s -> print_IFL_path p ^ s) iflpath ""
      ^ " must be "
      ^ List.fold_right (fun p s -> print_IFL_path p ^ s) iflpath' ""
      ^ "\n"

let node_minor node node' = node = node' || node' = Any

let arrow_minor arrow arrow' = arrow = arrow' || arrow' = LONGARROW

let modality_minor modality modality' =
  modality' = [ "any-mod" ]
  ||
  let modality = SS.of_list modality and modality' = SS.of_list modality' in
  SS.subset modality modality'

let marrow_minor (m, a) (m', a') = modality_minor m m' && arrow_minor a a'

let step_minor (node1, marrow1, node2) (node1', marrow1', node2') =
  node_minor node1 node1'
  && marrow_minor marrow1 marrow1'
  && node_minor node2 node2'

let rec kind_minor kind kind' =
  let pairs = make_pairs kind kind' in
  if List.length kind = 1 && List.length kind' = 1 then
    step_minor (List.hd kind) (List.hd kind')
  else
    (match kind' with
    | [ (n1', (m1', LONGARROW), n2') ] ->
        let n1, (m1, arrow1), n2 = List.hd kind
        and (nm, (mm, arrowm), nmu), kind'' = last_and_list (List.tl kind) in
        node_minor n1 n1' && node_minor nmu n2' && modality_minor m1 m1'
        && modality_minor mm m1'
        && List.for_all (fun (_, (mn, _), _) -> modality_minor mn m1) kind''
    | _ -> false)
    || (match (kind, kind') with
       | ( [ (n1, (m1, LONGARROW), n2); (_, (m2, SHORTARROW), n3) ],
           [ (n1', (m1', SHORTARROW), n2'); (_, (m2', LONGARROW), n3') ] ) ->
           node_minor n1 n1' && node_minor n3 n3'
           && n2' = Any
           && modality_minor m1 m1' && modality_minor m2 m2'
           && modality_minor m1 m2'
       | ( [ (n1, (m1, SHORTARROW), n2); (_, (m2, LONGARROW), n3) ],
           [ (n1', (m1', LONGARROW), n2'); (_, (m2', SHORTARROW), n3') ] ) ->
           node_minor n1 n1' && node_minor n3 n3'
           && n2' = Any
           && modality_minor m1 m1' && modality_minor m2 m2'
           && modality_minor m2 m1'
       | _ -> false)
    || List.exists
         (fun ((kind1, kind1'), (kind2, kind2')) ->
           kind_minor kind1 kind1' && kind_minor kind2 kind2')
         pairs

let minor iflreq iflreq' =
  match (iflreq, iflreq') with
  | E_MUST iflreq, E_MUST iflreq' -> kind_minor iflreq iflreq'
  | E_MUSTNOT iflreq, E_MUSTNOT iflreq' -> kind_minor iflreq' iflreq
  | E_EVERYMUST (iflreq1, iflreq2), E_EVERYMUST (iflreq1', iflreq2') ->
      kind_minor iflreq1' iflreq1 && kind_minor iflreq2 iflreq2
  | _ -> false

let meet iflreq iflreq' =
  if minor iflreq iflreq' then iflreq' else 
    if minor iflreq' iflreq then iflreq else
      raise (UncorrectRefinement "")