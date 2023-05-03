%{
  let error start stop =
    Errors.syntax Position.(of_lexing_pos start, of_lexing_pos stop)
%}

%token <string> IDEN
%token <Surface.constr> CONSTR_IDEN
%token FUN
%token MATCH
%token WITH
%token TYPE
%token LET
%token IN
%token OF
%token ARROW
%token PIPE
%token LPAREN
%token RPAREN
%token COMMA
%token EQUAL
%token COLON
%token DOUBLE_SEMI_COLON
%token EOF

%start <Surface.t> prog

%%

prog:
| decs = separated_list(DOUBLE_SEMI_COLON, declaration) EOF { decs }
| error { error $startpos $endpos }

declaration:
| LET; x = term_id; tyo = option(ty_annot); EQUAL; e = term
    { Surface.Val (x, tyo, e) }
| TYPE; id = ty_id; EQUAL; option(PIPE) cs = separated_nonempty_list(PIPE, constr_dec)
    { Surface.Enum (id, cs) }
| TYPE; id = ty_id; EQUAL; ty = ty
    { Surface.Alias (id, ty) };

constr_dec:
| id = CONSTR_IDEN
    { (id, []) }
| id = CONSTR_IDEN OF params = nonempty_list(ty)
    { (id, params) };

pattern:
| x = term_id
    { Surface.Pvar x }
| LPAREN; ps = separated_list(COMMA, pattern); RPAREN
    { Surface.Ptuple ps }
| p = pattern; ty = ty_annot
    { Surface.Pannot (p, ty) };

term_id:
| s = IDEN { Surface.V.of_string s }

term:
| LPAREN; xs = separated_list(COMMA, term); RPAREN
    { Surface.Tuple xs }
| LPAREN; e = term; RPAREN
    { e }
| x = term_id
    { Surface.Var x }
| FUN; b = binding
    { Surface.Lam b }
| k = CONSTR_IDEN; terms = list(term)
    { Surface.Constr (k, terms) }
| MATCH; e = term; WITH; bs = separated_nonempty_list(PIPE, binding)
    { Surface.Match (e, bs) }
| LET; p = pattern; EQUAL; e1 = term; IN; e2 = term
    { Surface.Let (e1, (p, e2)) }
| e = term; t = ty_annot
    {  Surface.Annot (e, t) };

binding:
| p = pattern ARROW e = term
    { (p, e) }

ty_id:
| s = IDEN { Type.V.of_string s }

ty:
| id = ty_id { Type.Named id }

ty_annot:
| COLON ty = ty { ty }
