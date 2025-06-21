# LS
* fix the leading keyword in a unit getting highlighted wrong
* pattern value annotations
* cast value annotations

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
