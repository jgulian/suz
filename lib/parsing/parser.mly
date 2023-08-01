%{
  open Structure
%}

%token FUN
%token EXT
%token LET
%token IF
%token WHILE
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token COLON
%token SCOLON
%token COMMA
%token EQUALS
%token DEQUALS
%token ADD
%token SUB
%token ARROW
%token EOF

%token <Structure.Ast.expression> NUM_LITERAL
%token <string> IDENTIFIER
%token <Structure.Ast.data_type> NUMERIC_TYPE

%type <Structure.Ast.item list> items
%type <Structure.Ast.item> item
%type <string * Structure.Ast.data_type> parameter
%type <Structure.Ast.code_block> code_block
%type <Structure.Ast.statement list * Structure.Ast.expression> statements
%type <Structure.Ast.statement> statement
%type <Structure.Ast.expression> expression
%type <Structure.Ast.binary_operation> binary_operation
%type <Structure.Ast.data_type> data_type

%left ADD SUB DEQUALS (* This might be wrong for DEQUALS *)

%start items

%%

items:
  | item* EOF { $1 }
  ;

item:
  | FUN name=IDENTIFIER LPAREN parameters=separated_list(COMMA, parameter); RPAREN ARROW return_type=data_type; body=code_block; 
    { Ast.Function ({ Ast.Function.name = name; parameters; return_type; body; location = Ast.li $loc; }, Ast.li $loc) }
  | EXT name=IDENTIFIER; LPAREN parameters=separated_list(COMMA, data_type); RPAREN ARROW return_type=data_type; SCOLON 
    { Ast.Extern ({ Ast.Extern.name = name; parameters; return_type; location = Ast.li $loc; }, Ast.li $loc) }
  ;

parameter:
  | IDENTIFIER COLON data_type { $1, $3 }
  ;

code_block:
  | LBRACK statements RBRACK { let (a, b) = $2 in (a, b, Ast.li $loc) }
  ;

statements:
  | expression { ([], $1) }
  | statement statements { let (lst, expr) = $2 in $1 :: lst, expr }

%inline statement:
  | LET IDENTIFIER COLON data_type EQUALS expression SCOLON { Ast.Assignment ($2, $4, $6, Ast.li $loc) }
  | expression SCOLON { Ast.Expression ($1, Ast.li $loc) }
  | IF condition=expression; body=code_block { Ast.If (condition, body, Ast.li $loc)  }
  | WHILE condition=expression; body=code_block { Ast.While (condition, body, Ast.li $loc) }
  ;

expression:
  | expression binary_operation expression { Binary ($1, $3, $2, Ast.li $loc) }
  | IDENTIFIER LPAREN separated_list(COMMA, expression) RPAREN { Call ($1, $3, Ast.li $loc) }
  | LPAREN expression RPAREN { $2 }
  | IDENTIFIER { Variable ($1, Ast.li $loc) }
  | NUM_LITERAL { $1 }
  | LPAREN RPAREN { VoidData (Ast.li $loc) }
  ;

%inline binary_operation:
  | ADD { Ast.Add (Ast.li $loc) }
  | SUB { Ast.Sub (Ast.li $loc) }
  | DEQUALS { Ast.Equals (Ast.li $loc) }
  ;

data_type:
  | LPAREN RPAREN { Ast.Void (Ast.li $loc) }
  | NUMERIC_TYPE { $1 }
  ;
