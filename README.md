# Blink

A pure OCaml HTTP client for Riot, inspired by Elixir's [Mint][mint]. It serves
as a lower-level library for handling HTTP connections within a single process. 

[mint]: https://github.com/elixir-mint/mint/

## Getting Started

Here's a basic example where we fetch the [OCaml.org][ocaml] website.

[ocaml]: https://ocaml.org

First we create a connection, which we can reuse:

<!-- $MDX file=./test/get_ocaml_org_test.ml,part=conn -->
```ocaml
let url = Uri.of_string "https://ocaml.org" in
let* conn = Blink.connect url in
```

This figures out the protocol that we will use, and returns a `conn` value that
we make requests with:

<!-- $MDX file=./test/get_ocaml_org_test.ml,part=request -->
```ocaml
let req = Http.Request.make "/" in
let* conn = Blink.request conn req () in
```

Finally, once we have made a request, we can call `Blink.stream` to
stream-parse the results and receive the parts as they come:

```ocaml
let rec run conn =
  let* conn, msgs = Blink.stream conn in
  match msgs with
  | [ `Done ] -> ()
  | msgs -> handle_messages msgs; run conn
in
let* () = run conn in
(* ... *)
```

Or if we prefer to consume all messages and pull together a response, we can do so with `Blink.await`:

<!-- $MDX file=./test/get_ocaml_org_test.ml,part=stream -->
```ocaml
let* _conn, response, body = Blink.await conn in
```
