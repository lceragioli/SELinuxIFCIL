open IFCILconfiguration
open CILsyntax
open Utils
module SS = Set.Make (String)

exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

let node_minor node node' = node = node' || node' = [ "any-node" ]

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
           && n2' = [ "any-node" ]
           && modality_minor m1 m1' && modality_minor m2 m2'
           && modality_minor m1 m2'
       | ( [ (n1, (m1, SHORTARROW), n2); (_, (m2, LONGARROW), n3) ],
           [ (n1', (m1', LONGARROW), n2'); (_, (m2', SHORTARROW), n3') ] ) ->
           node_minor n1 n1' && node_minor n3 n3'
           && n2' = [ "any-node" ]
           && modality_minor m1 m1' && modality_minor m2 m2'
           && modality_minor m2 m1'
       | _ -> false)
    (* let (n1, (m1, arrow1), n2) = List.hd kind
         and ((nm, (mm, arrowm), nmu), kind2) = last_and_list (List.tl kind)
         and (n1', (m1', arrow1'), n2') = List.hd kind'
         and ((nm', (mm', arrowm'), nmu'), kind2') = last_and_list (List.tl kind')
         in
         arrow1 = LONGARROW && arrow1' = SHORTARROW && arrowm = SHORTARROW && arrowm' = LONGARROW &&
         n2' = ["any-node"] && nm' = ["any-node"] &&
         node_minor n1 n1' && node_minor nm nm' && modality_minor m1 m1' && modality_minor m1 mm' && modality_minor mm mm' &&
         List.for_all (fun (_ , (m, _), _) -> modality_minor m mm') kind2
       ) *)
    || List.exists
         (fun ((kind1, kind1'), (kind2, kind2')) ->
           kind_minor kind1 kind1' && kind_minor kind2 kind2')
         pairs

let node_meet node node' =
  if node = node' then node
  else if node = [ "any-node" ] then node'
  else if node' = [ "any-node" ] then node
  else
    raise
      (UndefinedMeet
         ("between nodes " ^ String.concat "." node ^ "and "
        ^ String.concat "." node'))

let arrow_meet arrow arrow' =
  if arrow = LONGARROW then arrow'
  else if arrow' = LONGARROW then arrow
  else arrow

let modality_meet modality modality' =
  if modality = [ "any-mod" ] then modality'
  else if modality' = [ "any-mod" ] then modality
  else
    let modality = SS.of_list modality and modality' = SS.of_list modality' in
    let modality'' = SS.inter modality modality' in
    if SS.is_empty modality'' then
      raise
        (UndefinedMeet
           ("between modalities "
           ^ String.concat ", " (SS.elements modality)
           ^ "and "
           ^ String.concat ", " (SS.elements modality)))
    else SS.elements modality''

let marrow_meet (m, a) (m', a') = (modality_meet m m', arrow_meet a a')

let step_meet (node1, marrow1, node1') (node2, marrow2, node2') =
  let node3 = node_meet node1 node2
  and node3' = node_meet node1' node2'
  and marrow3 = marrow_meet marrow1 marrow2 in
  (node3, marrow3, node3')

(* let rec kind_meet_1 [(n1, marrow1, n1')] kind =
   let (n2, marrow2, n2') = List.hd kind
   and ((n3, marrow3, n3'), kind') = last_and_list (List.tl kind)
   in
   match marrow1 with
   | (_, SHORTARROW) -> raise (UndefinedMeet "")
   | (_, LONGARROW) ->
       ((node_meet n1 n2), (marrow_meet marrow1 marrow2), n2') ::
       (if kind' = [] then [] else
         (kind_meet_1 [(["any-node"], marrow1, ["any-node"])] kind'))
       @ [(n3, (marrow_meet marrow1 marrow3), (node_meet n1' n3'))] *)

let rec kind_meets kind kind' =
  let kind_meet_1 [ (n1, marrow1, n1') ] kind =
    let n2, marrow2, n2' = List.hd kind
    and (n3, marrow3, n3'), kind' = last_and_list (List.tl kind) in
    match marrow1 with
    | _, SHORTARROW -> raise (UndefinedMeet "")
    | _, LONGARROW ->
        if kind' = [] then
          [
            [
              (node_meet n1 n2, marrow_meet marrow1 marrow2, n2');
              (n3, marrow_meet marrow1 marrow3, node_meet n1' n3');
            ];
          ]
        else
          List.map
            (fun mt ->
              (node_meet n1 n2, marrow_meet marrow1 marrow2, n2') :: mt
              @ [ (n3, marrow_meet marrow1 marrow3, node_meet n1' n3') ])
            (kind_meets [ ([ "any-node" ], marrow1, [ "any-node" ]) ] kind')
  in
  if List.length kind == 1 && List.length kind' == 1 then
    [ [ step_meet (List.hd kind) (List.hd kind') ] ]
  else if List.length kind == 1 then kind_meet_1 kind kind'
  else if List.length kind' == 1 then kind_meet_1 kind' kind
  else
    let pairs = make_pairs kind kind' in
    let n1, marrow, n2 = List.hd kind
    and n1', marrow', n2' = List.hd kind'
    and rkind = List.tl kind
    and rkind' = List.tl kind' in
    (if snd marrow = LONGARROW && snd marrow' = SHORTARROW && n2 <> n2' then
     try
       (* print_string ((String.concat "." n2) ^ " with1 " ^ (String.concat "." n2')); *)
       let meets1 =
         kind_meets [ (n1, marrow, [ "any-node" ]) ] [ (n1', marrow', n2') ]
       and meets2 = kind_meets ((n2', marrow, n2) :: rkind) rkind' in
       List.map (fun (m1, m2) -> m1 @ m2) (times_list meets1 meets2)
     with UndefinedMeet s -> []
    else [])
    @ (if snd marrow = SHORTARROW && snd marrow' = LONGARROW && n2 <> n2' then
       try
         (* print_string ((String.concat "." n2) ^ " with2 " ^ (String.concat "." n2')); *)
         let meets1 =
           kind_meets [ (n1, marrow, n2) ] [ (n1', marrow', [ "any-node" ]) ]
         and meets2 = kind_meets rkind ((n2, marrow', n2') :: rkind') in
         List.map (fun (m1, m2) -> m1 @ m2) (times_list meets1 meets2)
       with UndefinedMeet s -> []
      else [])
    @ List.flatten
        (List.map
           (fun ((kind1, kind1'), (kind2, kind2')) ->
             try
               let meets1 = kind_meets kind1 kind1'
               and meets2 = kind_meets kind2 kind2' in
               List.map (fun (m1, m2) -> m1 @ m2) (times_list meets1 meets2)
             with UndefinedMeet _ -> [])
           pairs)

let kind_meet kind kind' =
  match remove_duplicate (kind_meets kind kind') with
  | [] -> raise (UndefinedMeet "")
  | [ m ] -> m
  | ms ->
      raise
        (NotUniqueMeet
           (String.concat " and "
              (List.map
                 (fun k -> List.fold_right (fun p s -> print_IFL p ^ s) k "")
                 ms)))

let meet iflreq iflreq' =
  match (iflreq, iflreq') with
  | MUST iflreq, MUST iflreq' -> MUST (kind_meet iflreq iflreq')
  | _ -> raise (UndefinedMeet "")

let minor iflreq iflreq' =
  match (iflreq, iflreq') with
  | MUST iflreq, MUST iflreq' -> kind_minor iflreq iflreq'
  | MUSTNOT iflreq, MUSTNOT iflreq' -> kind_minor iflreq' iflreq
  | EVERYMUST (iflreq1, iflreq2), EVERYMUST (iflreq1', iflreq2') ->
      kind_minor iflreq1' iflreq1 && kind_minor iflreq2 iflreq2
  | _ -> false

