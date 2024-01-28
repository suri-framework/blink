(lang dune 3.11)
(using mdx 0.4)

(name blink)

(generate_opam_files true)

(source
 (github leostera/blink))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name blink)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   (trail (>= "0.0.1"))
   (angstrom (>= "0.15.0"))
   (castore (>= "0.0.2"))
   (httpaf (>= "5.3.1"))
   (faraday (>= "0.8.2"))
   (http (>= "6.0.0~alpha2"))
   (mdx (and :with-test (>= "2.3.1")))
   (mirage-crypto-rng (>= "0.11.2"))
   (ocaml (>="5.1"))
   (odoc (and :with-doc (>= "2.2.2")))
   (riot (>= "0.0.8"))
   (x509 (>= "0.16.5"))
   dune)
 (tags
  (http client "http client" riot multicore)))
