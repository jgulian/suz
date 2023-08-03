open Core
open Llvm
open Structure
open Tsr
open Data_type

let codegen_binary_instruction op ty =
  match (op, ty) with
  | Ast.Add _, Numeric (Floating, _) -> build_fadd
  | Ast.Add _, _ -> build_add
  | Ast.Sub _, Numeric (Floating, _) -> build_fsub
  | Ast.Sub _, _ -> build_sub
  | Ast.Mul _, Numeric (Floating, _) -> build_fmul
  | Ast.Mul _, _ -> build_mul
  | Ast.Equals _, Numeric (Floating, _) -> build_fcmp Fcmp.Oeq
  | Ast.Equals _, _ -> build_icmp Icmp.Eq
  | Ast.NotEquals _, Numeric (Floating, _) -> build_fcmp Fcmp.One
  | Ast.NotEquals _, _ -> build_icmp Icmp.Ne
  | Ast.Less _, Numeric (Floating, _) -> build_fcmp Fcmp.Olt
  | Ast.Less _, Numeric (Signed, _) -> build_icmp Icmp.Slt
  | Ast.Less _, Numeric (Unsigned, _) -> build_icmp Icmp.Ult
  | Ast.Greater _, Numeric (Floating, _) -> build_fcmp Fcmp.Ogt
  | Ast.Greater _, Numeric (Signed, _) -> build_icmp Icmp.Sgt
  | Ast.Greater _, Numeric (Unsigned, _) -> build_icmp Icmp.Ugt
  | _, _ -> raise (Internal_issue "attempted to build unsupported type")
