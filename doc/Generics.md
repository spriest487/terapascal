Types can be declared as generic by including a type parameter list in their name.

```pascal
type
    GenericRecord[T] = record
        value: T;
    end;
    
    GenericVariant[TFirst, TSecond] = variant
        First: TFirst;
        Second: TSecond;
    end;
```

Generic types can be instantiated as normal. The type arguments used can either be inferred from the context,
or provided explicitly within an expression using a `with` clause.

The `with` clause is never part of the type name, and is only used with in expressions to disambiguate between
the index operator and the start of a type list.

```pascal
// var type and constructor type args inferred from member values
var intRec := GenericRecord(value: 123);

// constructor type and type args inferred from var declaration type
var stringRec: GenericRecord[String] := (value: 'hello');

// can't infer both parts of a variant from construction,
// so we need to provide a type hint here:
var var1: GenericVariant[String, Boolean] := GenericVariant.First('Hello');

// or using a `with` clause to explicitly specialize a generic type in an expression:  
var var2 := GenericVariant with [String, Boolean].Second(false);  
```
