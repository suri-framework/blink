# Blink

A pure OCaml HTTP client for Riot, inspired by Elixir's [Mint][mint]. It serves as a lower-level library for handling HTTP sockets within a single process. 

[mint]: https://github.com/elixir-mint/mint/

## Getting Started

Here's a basic example where we fetch the [OCaml.org][ocaml] website.

[ocaml]: https://ocaml.org

First we create a connection, which we can reuse:

<!-- $MDX file=./test/get_ocaml_org_test.ml,part=conn -->
```ocaml
  let url = Uri.of_string "https://abstractmachines.dev" in
  let conn =
    match Blink.connect url with
    | Ok conn -> conn
    | Error err ->
        (match err with
        | `Closed -> Logger.error (fun f -> f "closed")
        | `Invalid_uri uri -> Logger.error (fun f -> f "invalid_uri %a" Uri.pp uri)
        | `Tls_error exn -> Logger.error (fun f -> f "tls_error %s" (Printexc.to_string exn))
        | `Timeout -> Logger.error (fun f -> f "timeout")
        | `Unix_error err -> Logger.error (fun f -> f "unix_error: %s" (Unix.error_message err)));
        sleep 0.1;
        Stdlib.exit 1
  in
```

This figures out the protocol that we will use, and returns a `conn` value that we make requests with:

<!-- $MDX file=./test/get_ocaml_org_test.ml,part=request -->
```ocaml
  let req = Http.Request.make "/" in
  let conn, _req_ref = Blink.request conn req () |> Result.get_ok in
```

Finally, once we have made a request, we can call `Blink.stream` to stream-parse the results and receive the parts as they come:

<!-- $MDX file=./test/get_ocaml_org_test.ml,part=stream -->
```ocaml
  let _conn, parts =
    match Blink.stream conn with Ok x -> x | Error err -> 
        (match err with
        | `Closed -> Logger.error (fun f -> f "closed")
        | `Response_parsing_error -> Logger.error (fun f -> f "response_parsing_error")
        | `Invalid_uri uri -> Logger.error (fun f -> f "invalid_uri %a" Uri.pp uri)
        | `Tls_error exn -> Logger.error (fun f -> f "tls_error %s" (Printexc.to_string exn))
        | `Timeout -> Logger.error (fun f -> f "timeout")
        | `Unix_error err -> Logger.error (fun f -> f "unix_error: %s" (Unix.error_message err)));
        sleep 0.1;
        Stdlib.exit 1

  in

  let[@warning "-8"] [ `Status status; `Headers headers; `Data body; `Done ] =  parts in
```

When you receive a `` `Done `` message you'd have reached the end of the stream.
