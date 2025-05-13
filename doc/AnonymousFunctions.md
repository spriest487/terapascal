Anonymous functions can be written in two different forms: a compact form using the `lambda` keyword, 
or a longer form using the `procedure` or `function` keywords.

## Lambda syntax

Anonymous functions written using `lambda` may have an argument list, but cannot explicitly specify a result type
and instead have a body consisting of a single expression, from which the result type of the function is inferred.

```pascal
// the result type is inferred to be `Integer`, the type of `x + y`
var f := lambda(x: Integer; y: Integer): x + y;`
var y := f(1, 2); // y = 3
```

If the type of the parameters can be inferred from the context, they can be omitted:

```pascal
var f: function(Integer; Integer): Integer := lambda(x; y): x + y;
```

## Function syntax

Anonymous functions written using the `function` or `procedure` keywords are written like a normal function
declaration and must specify all their parameter types and result type. `procedure` can optionally be used to
indicate that the function has no result type, although using the `function` keyword and omitting the result type
has the same effect.

The body of this type of anonymous function must always be a block, delimited by the `begin` and `end` keywords 
as normal.

```pascal
var f := function(x, y: Integer): Integer;
    begin
        x + y
    end;
    
var p := procedure(x: Integer);
    begin
        WriteLn(x.ToString);
    end;
```


## Function types and objects

Both forms produce the same result: a function object. Function objects have a type that can be written similarly
to the normal syntax for declared functions:

```pascal
// function with no parmeters or result type
var f1: function;

// with parameters and no return type
var f2: function(Integer; Integer);

// parameter names are optional and not part of the type
// but you can use them for documentation
var f3: function(x, y: Integer); 

// with return type
var f4: function: Integer;
var f5: function(Integer): Integer;
```

Function objects are reference types. Their bodies can reference variables from outside their own scope. Any variables
referenced in this way are copied into the scope of the function body at the point the function is instantiated.

As a demonstration, the following example prints "1", because the value of `x` is copied into the scope of the
function stored at `f`, which is created before the value of `x` is incremented.

```pascal
var x := 1;
var f := function;
    begin
        WriteLn(x.ToString);
    end;

x += 1;

f();
```

The values of captured variables are stored in a closure object on the heap, allocated as part of the function object.
Functions that do not capture any outside variables do not have a closure and can be instantiated without allocating
memory.
