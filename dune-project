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
   (ocaml (>="5.1"))
   (riot (>= "0.0.6"))
   (http (>= "6.0.0~alpha2"))
   (httpaf (>= "0.7.1"))
   (faraday (>= "0.8.2"))
   (angstrom (>= "0.15.0"))
   (tls (>="0.17.3"))
   (x509 (>= "0.16.5"))
   (mirage-crypto-rng (>= "0.11.2"))
   (mdx (and :with-test (>= "2.3.1")))
   (odoc (and :with-doc (>= "2.2.2"))))
 (tags
  (http client "http client" riot multicore)))
