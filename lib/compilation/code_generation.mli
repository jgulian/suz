open Llvm
open Structure

val generate_code : string -> Tsr.build_module -> llcontext -> llmodule

(* ~/build/llvm-project/llvm/build/bin/llc *)
(* clang -S -emit-llvm runtime/io.c *)
