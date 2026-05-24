# impls rework
* update CIL: remove impls from interface defs, add new map

# refcounting cleanup pass
* taking the address of any type containing rc refs should be unsafe

# array rework
* C implementation could use variable sized structs instead of 2 allocations per array
* strings should be implemented as arrays

# RTTI
* more detailed type kind info
* type/method unit name info
* published methods/types shouldn't ever be stripped

# IR
* generics
* include consts in generated libs
* remove forward types from metadata
* undefined symbol checking can probably be moved to IR codegen rather than during typechecking
* make IR function src spans optional?? some builtins don't have them

# features
* "or" and "and" in patterns
* property syntax
* numeric compiler error codes
* compound assignment bitwise ops
* IsAbstract property for TypeInfo/MethodInfo
* move RTTI stuff into its own unit??
* overload support for MethodInfo/FunctionInfo

# LS
* cast value annotations
* partial tokenization
* partial typechecking

# bugs
* parser: newlines/escape chars in string literals
* signed i8 loop from -127 to 128 runs forever?
* can't infer generic args from object ctors

# improvements
* use tags instead of pascal-style modifiers for external etc
* add a named type case to typ::Type for all the things that use full_path, full_name etc
