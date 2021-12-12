let in_file = "NuSMVconfiguration"

let out_file = "NuSMVoutput"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result = CILgrammar.main CILlexer.token lexbuf in
  let normal = Normalization.normalize (Preprocessing.flatten_conf result [ "#" ]) in
  Verification.verify normal in_file out_file

