
(* let rec genbm n m names = *)
  
let depth = 30
let tnames = depth - 1
let mnames = depth - 1
let bnames = depth - 1

(* let rec range' n m ls =
  if n > m then ls
  else (range' (n+1) m (n :: ls)) *)

let rec listproduct ls ls' =
  match ls with
    | [] -> []
    | l::ls'' ->
        List.rev_append
          (List.rev_map
            (fun l' -> (l, l'))
            ls')
          (listproduct ls'' ls')

let rec gendecl names f =
  match names with
    | [] -> []
    | name :: names' ->
        List.rev_append 
          ((f name) :: (gendecl names' f))
          (gendecl names' f)

let genTypeDecl names =
  gendecl names (fun n -> "(type " ^ n ^ ")\n")

let genAttrDecl names =
  gendecl names (fun n -> "(typeattribute " ^ n ^ ")\n")
  

let range n m =
  if ((1 + m) - n) < 0 then [] else
  List.init 
    ((1 + m) - n)
    (fun x -> n + x)


(* let rec genNm n =
  String.make n 'a' *)

let rec genRs' nb na names =
  
  List.concat
    (List.rev_map
      (fun (bname, (nb', na')) -> 
        (List.rev_map
          (fun str -> ("(block " ^ bname ^ "\n" ^ str))
          (genRs nb' na' names)
        )
      )  
    (listproduct
      names
      (listproduct
        (range 1 (nb-1))
        (range 1 (na-1)))))

and genRs nb na names =
  if nb + na <= 0 then [""]
  else
    let typedec = (genTypeDecl names)
    and attrdec = (genAttrDecl names)
  in
    List.rev_append
      typedec
      (List.rev_append
        attrdec
        (genRs' nb na names)
      )

(* and genType' n m =
  let nm = genNm m
    in
    List.rev_map
      (fun s -> 
        s ^ "(type " ^ nm ^ ")\n")
      (genRs (n-1))

and genType n =
  List.fold_left
    (fun ls m -> List.rev_append (genType' (n-m) m) ls)
    []
    (range 1 (n-1))

and genAttr' n m =
  let nm = genNm m
    in
    List.rev_map
      (fun s -> 
        s ^ "(typeattribute " ^ nm ^ ")\n")
      (genRs (n-1))
and genAttr n =
  List.fold_left
  (fun ls m -> List.rev_append (genAttr' (n-m) m) ls)
  []
  (range 1 (n-1)) *)

(* and genAttrSet' m p n s =
  let mn = genNm m and
  pn = genNm p
    in
    genRs (n-1) (s ^ "(attributeset " ^ mn ^ " ("^ pn ^"))\n");
    
and genAttrSet n s =
  for c = 1 to (n - 1) do
    for p = (c + 1)  to (n - 1) do
      genAttrSet' c (p - c) (n - p) s
    done
  done

and genInherit' m n s =
  let nm = genNm m
  in
  genRs (n-1) (s ^ "(blockinherit " ^ nm ^ ")\n");
    
and genInherit n s =
  for m = 1 to (n - 1) do
    genInherit' m (n-m) s;
  done

and genCall' m p n s =
  let mn = genNm m and
  pn = genNm p
    in
    genRs (n-1) (s ^ "(call " ^ mn ^ " ("^ pn ^"))\n");
    
and genCall n s =
  for c = 1 to (n - 1) do
    for p = (c + 1)  to (n - 1) do
        genCall' c (p - c) (n - p) s
    done
  done

and genAllow' m p n s =
  let mn = genNm m and
  pn = genNm p
    in
    genRs (n-1) (s ^ "(allow " ^ mn ^ " "^ pn ^"(file (read)))\n");
    
and genAllow n s =
  for c = 1 to (n - 1) do
    for p = (c + 1)  to (n - 1) do
      genAllow' c (p - c) (n - p) s
    done
  done *)

let names num =
  List.init 
    num
    (fun i -> String.make 1 (Char.chr (i + 97)))

let _ =
  let blockNum = int_of_string(Sys.argv.(1)) 
  and allowNum = int_of_string(Sys.argv.(2)) 
  and nameNum = int_of_string(Sys.argv.(3)) 
  in
  List.iter
    (fun st -> print_string (st ^ "\n\n"))
    (genRs blockNum allowNum (names nameNum))



