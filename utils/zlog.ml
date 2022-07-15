module Log = Dolog.Log

let init () =
  Log.set_log_level Log.DEBUG;
  Log.set_output stdout;
  ()
