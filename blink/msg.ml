open Riot

type message =
  [ `Data of IO.Buffer.t
  | `Done
  | `Headers of Http.Header.t
  | `Status of Http.Status.t ]
