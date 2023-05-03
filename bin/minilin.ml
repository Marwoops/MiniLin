let parse_surface ic =
  let lexbuf = Lexing.from_channel ic in
  Minilin.Parser.prog Minilin.Lexer.token lexbuf

let process_file filename =
  try
    let ic = open_in filename in
    let surface = parse_surface ic in
    close_in ic;
    Format.printf "%a@?" Minilin.Surface.pp surface
  with
  | Minilin.Errors.Error cause ->
     Format.eprintf "%a@." Minilin.Errors.pp cause

let _ =
  let open Arg in
  Arg.parse
    [
    ]
    process_file
    "minilin: [OPTIONS] file1 ... fileN"
