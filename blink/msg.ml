open Riot

type message =
  [ `Data of Bytestring.t
  | `Done
  | `Headers of Http.Header.t
  | `Status of Http.Status.t ]
