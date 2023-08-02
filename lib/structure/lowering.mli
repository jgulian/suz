val lower : Ast.item list -> Tsr.build_module
val map_op_to_name : Ast.binary_operation -> Tsr.data_type -> string
val op_return_type : Ast.binary_operation -> Tsr.data_type -> Tsr.data_type
