open Base

type 'data t =
  { event_data : 'data
  ; session : Session.t
  ; source : string
  ; time : float
  }

let create ~(source : string) ~(session : Session.t) ~(event_data : 'data) : 'data t =
  let time = Unix.gettimeofday () in
  { event_data; session; source; time }
;;
