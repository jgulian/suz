%{
  open Syntax.Expression
  open Syntax.Type_system
%}

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> IDENTIFIER
%token FUN
%token STRUCT
%token ENUM
%token LET
%token MUT
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token LBLOCK
%token RBLOCK
%token LANGLE
%token RANGLE
%token COLON
%token SCOLON
%token COMMA
%token PERIOD
%token EQUALS
%token ADD
%token SUBTRACT
%token MULTIPLY
%token DIVIDE
%token ARROW
%token USIZE
%token ISIZE
%token FSIZE
%token EOF

%type <item list> items
%type <item> item
%type <function_definition> function_definition
%type <string * data_type> parameter
%type <code_block> code_block
%type <code_block> statements
%type <statement> statement
%type <assignment> assignment
%type <data_type> type_annotation
%type <expression> expression
%type <binary_expression> binary_expression
%type <binary_operation> binary_operation
%type <call_expression> call_expression
%type <struct_construction_expression> struct_construction_expression
%type <string * expression> struct_construction_parameter
%type <data_type> return_type
%type <struct_definition> struct_definition
%type <string * data_type> struct_field
%type <enum_definition> enum_definition
%type <string * data_type> enum_variant
%type <string list> generic_definition
%type <data_type> data_type

%left ADD SUBTRACT LANGLE RANGLE
%right MULTIPLY DIVIDE PERIOD

%start items

%%

items:
  | item* EOF { $1 }
  ;

item:
  | function_definition { FunctionDefinition $1 }
  | struct_definition { StructDefinition $1 }
  | enum_definition { EnumDefinition $1 }
  ;

function_definition:
  | FUN IDENTIFIER loption(generic_definition) LPAREN separated_list(COMMA, parameter) RPAREN return_type? code_block 
    { {name = $2; function_generics = $3; parameters = $5; return_type = Option.value $7 ~default:VoidType; block = $8; } }
  ;

(** Same as struct_field but whatever *)
parameter:
  | IDENTIFIER COLON data_type { $1, $3 }
  ;

code_block:
  | LBRACK statements RBRACK { $2 }
  ;

statements:
  | expression { ([], $1) }
  | statement statements { let (lst, expr) = $2 in $1 :: lst, expr }

%inline statement:
  | assignment SCOLON { Assignment $1 }
  | expression SCOLON { Expression $1 }
  ;

(** Support explicit type definition? *)
assignment: 
  | LET boption(MUT) IDENTIFIER ioption(type_annotation) EQUALS expression { $3, $6, $2 }
  ;

type_annotation:
  | COLON data_type { $2 }
  ;

expression:
  | binary_expression { BinaryExpression $1 }
  | call_expression { CallExpression $1 }
  | struct_construction_expression { StructConstructionExpression $1 }
  | LPAREN expression RPAREN { $2 }
  | IDENTIFIER { VariableExpression $1 }
  | INT_LITERAL { LiteralExpression (Number ({ format = Signed; byte_count = None; }, float_of_int $1)) }
  | FLOAT_LITERAL { LiteralExpression (Number ({ format = Float; byte_count = None; }, $1)) }
  ;

binary_expression:
  | expression binary_operation expression { { lhs = $1; op = $2; rhs = $3; } }
  ;

%inline binary_operation:
  | ADD { Add }
  | SUBTRACT { Sub }
  | MULTIPLY { Mul }
  | DIVIDE { Div }
  | LANGLE { Less }
  | RANGLE { Greater } 
  ;

call_expression:
  | expression PERIOD IDENTIFIER LPAREN separated_list(COMMA, expression) RPAREN { $3, List.cons $1 $5 }
  | IDENTIFIER LPAREN separated_list(COMMA, expression) RPAREN { $1, $3 }
  ;

struct_construction_expression:
  | IDENTIFIER LBRACK separated_list(COMMA, struct_construction_parameter) RBRACK { $1, $3 }
  ;

struct_construction_parameter:
  | IDENTIFIER COLON expression { $1, $3 }
  ;

return_type:
  | ARROW data_type { $2 }
  ;

struct_definition:
  | STRUCT IDENTIFIER loption(generic_definition) LBRACK separated_list(COMMA, struct_field) RBRACK 
    { {struct_name = $2; struct_generics = $3; fields = $5} }
  ;

struct_field:
  | IDENTIFIER COLON data_type { $1, $3 }
  ;

enum_definition:
  | ENUM IDENTIFIER loption(generic_definition) LBRACK separated_list(COMMA, enum_variant) RBRACK 
    { { enum_name = $2; enum_generics = $3; variants = $5; } }
  ;

enum_variant:
  | IDENTIFIER LPAREN data_type RPAREN { $1, $3 }
  ;

generic_definition:
  | LBLOCK separated_nonempty_list(COMMA, IDENTIFIER) RBLOCK { $2 }
  ;

data_type:
  | USIZE { NumericType { format = Unsigned; byte_count = None } }
  | ISIZE { NumericType { format = Signed; byte_count = None } }
  | FSIZE { NumericType { format = Float; byte_count = None } }
  | IDENTIFIER { NamedType $1 }
  | LPAREN separated_nonempty_list(COMMA, data_type) RPAREN { Tuple $2 }
  ;
