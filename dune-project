(lang dune 3.0)

(name ubjson_parser)

(generate_opam_files true)

(source
 (github packell1/ubjson_parser))

(authors "Liam Packer")

(maintainers "Liam Packer")

(license LICENSE)

(package
 (name ubjson_parser)
 (synopsis "A UBJSON parser for OCAML")
 (description "A Universal Binary JSON parsing library, powered by Angstrom, for parsing any data specified in the UBJSON format.")
 (depends ocaml dune angstrom))