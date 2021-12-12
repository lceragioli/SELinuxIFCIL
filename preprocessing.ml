open Utils
open IFCILconfiguration
open CILgrammar

exception OurError of string

exception UnsupportedConstruct of string

exception UndefinedReference of string

exception UndefinedMeet of string

exception NotUniqueMeet of string

exception UncorrectRefinement of string

type semi_flat_statement =
  | FLAT of flat_statement
  | SEMIFLATIN of path * statement list

let eval_semifl pos b smfstmntls =
  match b with
  | "#" :: bs -> b
  | _ ->
      let name, qual = last_and_list b in
      if
        List.exists
          (fun (ps, i) -> ps = pos @ qual && i = FLAT (FLATBLOCK name))
          smfstmntls
      then pos @ b
      else
        raise
          (UndefinedReference
             ("when flattening: block " ^ String.concat "." (pos @ b)))

let rec flatten_conf stmnts loc =
  let simplifyin stmnts =
    List.map
      (fun stmnt ->
        match stmnt with
        | CILIN (path, stmnts) ->
            let path = if List.hd path = "#" then List.tl path else path in
            let name, qual = last_and_list path in
            List.fold_right
              (fun name stm ->
                CILBLOCK (name, [ stm ]))
              qual
              (CILBLOCK (name, stmnts))
        | _ -> stmnt)
      stmnts
  in
  let rec semi_flatten_conf stmnts loc =
    List.fold_left
      (fun semiflatls stmnt -> semiflatten_st stmnt loc @ semiflatls)
      [] stmnts
  and semiflatten_st stmnt loc =
    match stmnt with
    | CILTYPE t -> [ (loc, FLAT (FLATTYPE t)) ]
    | CILTYPEALIAS t -> [ (loc, FLAT (FLATTYPEALIAS t)) ]
    | CILTYPEALIASACTUAL (a, t) -> [ (loc, FLAT (FLATTYPEALIASACTUAL (a, t))) ]
    | CILATTRIBUTE t -> [ (loc, FLAT (FLATATTRIBUTE t)) ]
    | CILATTRIBUTESET (name, attrset) ->
        [ (loc, FLAT (FLATATTRIBUTESET (name, attrset))) ]
    | CILBLOCK (name, stmnts) ->
        (loc, FLAT (FLATBLOCK name)) :: semi_flatten_conf stmnts (loc @ [ name ])
    | CILBLOCKINHERIT (t, i) -> [ (loc, FLAT (FLATBLOCKINHERIT (t, i))) ]
    | CILBLOCKABSTRACT -> [ (loc, FLAT FLATBLOCKABSTRACT) ]
    | CILCALL (n, p, i) -> [ (loc, FLAT (FLATCALL (n, p, i))) ]
    | CILMACRO (name, params, stmnts) ->
        (loc, FLAT (FLATMACRO (name, params)))
        :: semi_flatten_conf stmnts (loc @ [ name ])
    | CILALLOW (src, trg, clsperm) ->
        [ (loc, FLAT (FLATALLOW (src, trg, clsperm))) ]
    | CILIN (path, stmnts) -> [ (loc, SEMIFLATIN (path, stmnts)) ]
    | CILCOMMON (name, o) -> [ (loc, FLAT (FLATCOMMON (name, o))) ]
    | CILCLASSCOMMON (cls, common) ->
        [ (loc, FLAT (FLATCLASSCOMMON (cls, common))) ]
    | CILCLASS (cls, o) -> [ (loc, FLAT (FLATCLASS (cls, o))) ]
    | CILCLASSPERMISSION name -> [ (loc, FLAT (FLATCLASSPERMISSION name)) ]
    | CILCLASSPERMISSIONSET (perms, cls, exp) ->
        [ (loc, FLAT (FLATCLASSPERMISSIONSET (perms, cls, exp))) ]
    | CILCLASSMAP (name, mappings) ->
        [ (loc, FLAT (FLATCLASSMAP (name, mappings))) ]
    | CILCLASSMAPPING (clsmap, clsmapping, clsperm) ->
        [ (loc, FLAT (FLATCLASSMAPPING (clsmap, clsmapping, clsperm))) ]
    | IFL (lbl, req) -> [ (loc, FLAT (FLATIFL (lbl, req))) ]
  and flatin (loc, stmnt) sfstmtnts =
    match stmnt with
    | SEMIFLATIN (blockpath, stmnts) ->
        let block =
          try eval_semifl loc blockpath sfstmtnts
          with UndefinedReference _ -> eval_semifl [ "#" ] blockpath sfstmtnts
        in
        List.rev_map
          (fun (p, fstm) ->
            match fstm with
            | FLAT s -> (p, s)
            | _ ->
                raise
                  (UnsupportedConstruct
                     "'in' statements inside 'in' statements are not allowed \
                      by CIL"))
          (semi_flatten_conf stmnts block)
    | FLAT f -> [ (loc, f) ]
  in
  let sstmnts = simplifyin stmnts in
  let semiflatstmnts = semi_flatten_conf sstmnts loc in
  List.fold_left
    (fun flatls semiflatstmnt -> flatin semiflatstmnt semiflatstmnts @ flatls)
    [] semiflatstmnts
