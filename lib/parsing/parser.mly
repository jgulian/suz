%{
  open Structure
  open Core
  open General
%}

%token FUN
%token EXT
%token LET
%token IF
%token WHILE
%token TYPE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token LBLOCK
%token RBLOCK
%token COLON
%token SCOLON
%token PERIOD
%token COMMA
%token EXCLAMATION
%token AMPERSAND
%token EQUALS
%token DEQUALS
%token NEQUALS
%token LANGLE
%token RANGLE
%token ADD
%token SUB
%token MUL
%token ARROW
%token ASSIGN
%token EOF

%token <Structure.Ast.expression> NUM_LITERAL
%token <string> IDENTIFIER
%token <Structure.Ast.data_type> NUMERIC_TYPE
%token <int> INDEX

%type <Structure.Ast.item list> items
%type <Structure.Ast.item> item
%type <string * bool * Structure.Ast.data_type> parameter
%type <Structure.Ast.code_block> code_block
%type <Structure.Ast.statement list * Structure.Ast.expression option> statements
%type <Structure.Ast.statement> statement
%type <Structure.Ast.expression> expression
%type <Structure.Ast.writable_expression> writable_expression
%type <Structure.Ast.location> location
%type <Structure.Ast.binary_operation> binary_operation
%type <Structure.Ast.expression> unary_expression
%type <Structure.Ast.data_type> data_type

%left ADD SUB DEQUALS NEQUALS LANGLE RANGLE (* This might be wrong for DEQUALS, NEQUALS, LANGLE, RANGLE *)
%right MUL EXCLAMATION

%start items

%%

items:
  | item* EOF { $1 }
  ;

item:
  | FUN name=IDENTIFIER LPAREN parameters=separated_list(COMMA, parameter); RPAREN ARROW return_type=data_type; body=code_block; 
    { Ast.Function ({ Ast.Function.name = name; parameters; return_type; body; location = li $loc; }, li $loc) }
  | EXT name=IDENTIFIER; LPAREN parameters=separated_list(COMMA, data_type); RPAREN ARROW return_type=data_type; SCOLON 
    { Ast.Extern ({ Ast.Extern.name = name; parameters; return_type; location = li $loc; }, li $loc) }
  | TYPE alias=IDENTIFIER; EQUALS ty=data_type SCOLON { Ast.Type (alias, ty, li $loc) }
  ;

parameter:
  | name=IDENTIFIER COLON ref=AMPERSAND? ty=data_type { name, Option.is_some ref, ty }
  ;

code_block:
  | LBRACK statements RBRACK { let (a, b) = $2 in (a, b, li $loc) }
  ;

statements:
  | statement { ([$1], None) } 
  | expression { ([], Some $1) }
  | statement statements { let (lst, expr) = $2 in $1 :: lst, expr }

%inline statement:
  | LET IDENTIFIER COLON data_type EQUALS expression SCOLON { Ast.Assignment ($2, $4, $6, li $loc) }
  | expression SCOLON { Ast.Expression ($1, li $loc) }
  | target=writable_expression; ASSIGN value=expression; SCOLON { Ast.Reassignment (target, value, li $loc) }
  | IF condition=expression; body=code_block { Ast.If (condition, body, li $loc)  }
  | WHILE condition=expression; body=code_block { Ast.While (condition, body, li $loc) }
  ;

expression:
  | unary_expression { $1 }
  | expression binary_operation expression { Binary ($1, $3, $2, li $loc) }
  | IDENTIFIER LPAREN separated_list(COMMA, expression) RPAREN { Call ($1, $3, li $loc) }
  | LPAREN factors=separated_list(COMMA, expression) RPAREN 
    { if List.length factors = 1 then List.nth_exn factors 0 else Ast.TupleBuild (factors, li $loc) }
  (* | expr=expression PERIOD index=INDEX { Ast.TupleAccess (expr, index, li $loc) } *)
  | writable_expression { Variable ($1, li $loc) }
  | NUM_LITERAL { $1 }
  ;

writable_expression:
  | var=IDENTIFIER; locations=list(location) { (var, locations, li $loc) }
  ;

location:
  | PERIOD index=INDEX { Ast.IndexAccess (index, li $loc) }
  | PERIOD field=IDENTIFIER { Ast.NamedAccess (field, li $loc) }
  | EXCLAMATION { Ast.DerefAccess (Literal (0., Some (Numeric (Unsigned, Some 8, li $loc)), li $loc), li $loc) }
  | LBLOCK index=expression RBLOCK { Ast.DerefAccess (index, li $loc) }
  ;

%inline binary_operation:
  | DEQUALS { Ast.Equals (li $loc) }
  | NEQUALS { Ast.NotEquals (li $loc) }
  | LANGLE { Ast.Less (li $loc) }
  | RANGLE { Ast.Greater (li $loc) }
  | ADD { Ast.Add (li $loc) }
  | SUB { Ast.Sub (li $loc) }
  | MUL { Ast.Mul (li $loc) }
  ;

%inline unary_expression:
  | EXCLAMATION inner=expression { Ast.Unary (inner, Not (li $loc), li $loc) }
  ;
  (*  Ast.Unary (inner, Deref (li $loc), li $loc) *)

data_type:
  | NUMERIC_TYPE { $1 }
  | LPAREN factors=separated_list(COMMA, data_type) RPAREN 
    { if List.is_empty factors then Ast.Void (li $loc) else Ast.Tuple (factors, li $loc) }
  | name=IDENTIFIER { Ast.Named (name, li $loc) }
  | MUL inner=data_type { Ast.Pointer (inner, li $loc) }
  ;
