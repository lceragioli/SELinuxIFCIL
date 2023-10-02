open IFCILsyntax
open Utils

module Block = struct
  type t = 
  { 
    rules: statement list;
    abstract: bool;
    nested: ns list;
  }

  let emptyframe = {rules = []; abstract = false; nested = []}

  let union { rules = r; abstract = a; nested = n } { rules = r'; abstract = a'; nested = n' } =
    {
      rules = List.rev_append r r';
      abstract = a || a';
      nested = List.rev_append n n';
    }
end

type commands = Block.t SLM.t 

let commands_union com com' =
  SLM.union
    (fun _ b b' -> Some (Block.union b b'))
    com
    com'

let rec from_config' rules ns =
  List.fold_left
  (fun xcom xcom' -> commands_union xcom xcom')
  SLM.empty
  (List.rev_map
    (fun rule -> from_rule rule ns)
    rules)

and from_rule rule ns =
  match rule with
    | CILALLOW (_, _, _) 
    | CILATTRIBUTESET (_, _) 
    | CILBLOCKINHERIT _ 
    | CILCALL (_, _) 
    | CILCOMMON (_, _)
    | CILCLASSCOMMON (_, _)
    | CILCLASS (_, _)
    | CILCLASSPERMISSION _
    | CILCLASSMAP (_, _)
    | CILCLASSMAPPING (_, _, _)
    | CILCLASSPERMISSIONSET (_, _, _)
    | CILTYPEALIASACTUAL (_, _) 
    | IFL (_, _) ->
          (SLM.add
            ns 
            ({rules = [rule]; abstract = false; nested = []} :Block.t)
            SLM.empty)
    | CILBLOCKABSTRACT ->
          (SLM.add
            ns 
            ({rules = []; abstract = true; nested = []} :Block.t)
            SLM.empty)
    | CILBLOCK (bdn, rules) ->
        let ns' = (List.rev (bdn::(List.rev ns))) in
        let coms = from_config' rules ns' in
        commands_union
          coms
        (commands_union
          (SLM.add
            ns'
            ({rules = []; abstract = false; nested = []} :Block.t)
            SLM.empty) 
          (SLM.add
            ns 
            ({rules = []; abstract = false; nested = [ns']} :Block.t)
            SLM.empty))
    | _ -> SLM.empty 

let from_config rules =
  from_config' rules ["#"]

let print comds =
  SLM.iter
    (fun ns (blck :Block.t) ->
      print_string "At ";
      print_string (String.concat "." ns);
      print_string " :\n";
      print blck.rules;
    )
    comds