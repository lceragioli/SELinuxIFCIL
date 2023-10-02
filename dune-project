(lang dune 2.9)

(name SELinuxIFCIL)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Lorenzo Ceragioli")

(maintainers "Lorenzo Ceragioli")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name SELinuxIFCIL)
 (synopsis "SELinux IFCIL configuration Language Semantics")
 (description "")
 (depends 
  ocaml 
  dune
  ocamllex
  ocamlyacc
  )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
