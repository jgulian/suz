# Suz (sʌzi) a programming langauge for experimentation

This is a programming language for me to implement plt ideas I find 
interesting. 
The compiler does some basic type-checking (although I have not implemented all 
the checking I want, so the whole system is very unsound).
The parsed Ast is converted into a Tsr which representes the code while 
simplifying syntatic sugar. 
The Tsr is then lowered to llvm ir which is compiled to the binary using clang.
Because of this, beyond the requirements presented, it also requires clang be 
present on the system.

The language currently supports 
 * Function calling
 * If/While statements
 * Foreign function interface
 * Pointer manipulation both dereferencing and array dereferencing
 * Tuple types, construction and accessing
 * Pass by value and pass by reference

I have several other features planned.
The codebase is also not performant. 
For instance, the code generation for expressions is very liberal in it's use 
of allocations.

Below is an example of the syntax. 
A lot more syntax can be found in the `test.suz` file.
Note that there is type aliasing, but it is not complete, so while they can be
defined, they can not be constructed or deconstructed.

```
ext read_int() -> usize;

ext print_int(usize) -> ();

ext newline() -> ();

fun main() -> () {
    let a: usize = read_int();
    let b: usize = double(a);
    let c: usize = double(b);
    let d: usize = a + b + c;
    print_int(d);

    newline();

    let e: usize = read_int();
    let e: usize = sub_one(e);
    print_int(e);

    newline();

    let f: usize = read_int();
    if f == 1_usize {
        print_int(23_usize);
    }
    print_int(42_usize);
}

fun double(a: usize) -> usize {
    a + a + a
}

fun sub_one(a: usize) -> usize {
    a - 2_usize
}
```