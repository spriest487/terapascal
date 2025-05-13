# features
* maybe get rid of 
* add a named type case to typ::Type for all the things that use full_path, full_name etc
* Object should be an interface?
  * auto-generated implementations of ToString and Equals for everything if they don't do it themselves
* explicit type args to func calls:
  * `MyFunc with [Integer]();`
  * can this be avoided if we do evil stuff to detect typenames in [] groups and give them special treatment
    so the typechecker can sort it out later?
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)
* property syntax
* numeric compiler error codes
* compound assignment bitwise ops

# bugs
* parser: newlines/escape chars in string literals
* signed i8 loop from -127 to 128 runs forever? 

# improvements
* go back to I prefix for interfaces
* undefined symbol checking can probably be moved to IR codegen rather than during typechecking 
* type cache so we don't need to construct/pass around type objects all the time in the typechecker (This is really slow)
* make IR function src spans optional?? some builtins don't have them
* reimplement dynarrays so they don't use a separate allocation for items
