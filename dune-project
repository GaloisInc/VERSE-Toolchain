(lang dune 3.16)

(name verse-toolchain)

(generate_opam_files true)

(authors "Sam Cowger")

(maintainers "Sam Cowger")

(package
 (name cn-lsp)
 (synopsis "CN Language Server")
 (description
  "A language server protocol implementation for the CN language")
 (depends
  dune
  (ocaml
   (and
    (>= 4.14.1)
    (< 6.0.0)))
  ;;
  (base
   (and
    (>= v0.16.3)
    (< v0.18)))
  (cerberus
   (= "b9daa22"))
  (cerberus-lib
   (= "b9daa22"))
  (cn
   (= "b9daa22"))
  (jsonrpc
   (and
    (>= 1.17.0)
    (< 2.0.0)))
  (linol
   (and
    (>= 0.6)
    (< 1.0)))
  (linol-lwt
   (and
    (>= 0.6)
    (< 1.0)))
  (logs
   (and
    (>= 0.7.0)
    (< 1.0.0)))
  (lsp
   (and
    (>= 1.17.0)
    (< 2.0.0)))
  telemetry))

(package
 (name telemetry)
 (synopsis "Telemetry collection")
 (allow_empty)
 (depends
  ocaml
  dune
  (alcotest
   (and
    (>= 1.6.0)
    (< 2.0.0)))
  (base
   (and
    (>= v0.16.3)
    (< v0.18)))
  (ppx_deriving_yojson
   (and
    (>= 3.0.0)
    (< 4.0.0)))
  (ppx_hash
   (and
    (>= v0.16.0)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.16.0)
    (< v0.18)))
  (yojson
   (and
    (>= 2.0.0)
    (< 3.0.0)))))
