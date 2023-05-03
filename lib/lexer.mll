{
    open Parser

    let current_pos lexbuf = Position.of_lexing_pos lexbuf.Lexing.lex_curr_p

    let kw_or_ident_of_string : string -> token =
      let kws = Hashtbl.create 10 in
      List.iter
        (fun (s, tok) -> Hashtbl.add kws s tok)
        [
          "fun", FUN;
          "match", MATCH;
          "with", WITH;
          "type", TYPE;
          "let", LET;
          "in", IN;
          "of", OF;
        ];
      fun s -> try Hashtbl.find kws s with Not_found -> IDEN s
}

let legal_iden = ['0'-'9' '_' 'a'-'z' 'A'-'Z']*
let constr_iden = ['A'-'Z'] legal_iden
let iden = ['a'-'z'] legal_iden
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule instr =
    parse
    | ([^';']* ";;") as instr
        { Some instr, true }
    | eof
        { None, false }

and token =
    parse
    | '\n' { Lexing.new_line lexbuf; token lexbuf }
    | white { token lexbuf }
    | "{*" { comment 1 lexbuf }
    | iden as s { kw_or_ident_of_string s }
    | constr_iden as s { CONSTR_IDEN s }
    | "->" {ARROW}
    | "|" { PIPE }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "," { COMMA }
    | "=" { EQUAL }
    | ":" { COLON }
    | ";;" { DOUBLE_SEMI_COLON }
    | eof { EOF }
    | _ { Errors.unknown_character (current_pos lexbuf) (Lexing.lexeme lexbuf) }

and comment n =
  parse
| "{*" { comment (n + 1) lexbuf }
| "*}" { if n = 1 then token lexbuf else comment (n - 1) lexbuf }
| eof { Errors.unterminated_comment (current_pos lexbuf) }
| _ { comment n lexbuf }
