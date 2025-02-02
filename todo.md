# critical
* project files/don't auto load used units
* destructors

# features
* generic interfaces 
* Object should be an interface?
  * need interface inheritance so Object can imply Comparable and Displayable
* @-attributes - replace `external` etc with these (eventually make them available at runtime but not now)
* reflection for published members
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
