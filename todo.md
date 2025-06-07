# LS
* files need to be read from in memory not disk to match in-progress edits
* simplify func/method typechecking a lot
  * the AST shouldn't be rewritten
  * sugar methods like string concatenation shouldn't be rewritten
  * more types of value nodes if necessary

# features
* add a named type case to typ::Type for all the things that use full_path, full_name etc
* function/method tags 
  * replace `external` etc with these
* property syntax
* numeric compiler error codes
* compound assignment bitwise ops
* IsAbstract property for TypeInfo/MethodInfo
* move RTTI stuff into its own unit??
* overload support for MethodInfo/FunctionInfo

# bugs
* parser: newlines/escape chars in string literals
* signed i8 loop from -127 to 128 runs forever?
* can't infer generic args from object ctors

# improvements
* undefined symbol checking can probably be moved to IR codegen rather than during typechecking 
* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* make IR function src spans optional?? some builtins don't have them
* reimplement dynarrays so they don't use a separate allocation for items
* named matchers e.g. "expected start of type name (one of: ...)"
