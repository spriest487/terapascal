Interface type declarations define an abstract type containing a set of methods. Concrete
types can implement an interface to declare that they implement all of its methods.

```pascal
type IAnimal = interface
    function Speak;
end;
```

A concrete type is considered to implement a method if it has a method in its own
declaration with the same name and a compatible signature.

```pascal
type Dog = class
    function Speak;
end;

function Dog.Speak;
begin
    WriteLn('Woof!');
end;
```

Interfaces can be used to implement polymorphism by standing in for any of their implementing
types, either dynamically at runtime, or as type constraints at compile
time.

## Dynamic polymorphism

Objects of reference type that implement an interface can be assigned to values of that interface
type.

```pascal
var animal: IAnimal := Dog();
animal.Speak; // "Woof!"

animal := Cat();
animal.Speak; // "Meow!"
```

## Compile-time polymorphism

Interfaces can be used in combination with generic methods to constrain the types passed as type arguments
to implementors of a particular interface.

```pascal
function AnimalSpeak[T](animal: T)
where 
    T is IAnimal;
begin
    animal.Speak;
end;
```

## `Self` type

Interfaces declarations can make use of the `Self` type, which is an undefined, unsized
placeholder type that stands in the for the implementing type.

For example:

```pascal
type 
    IAddToSelf = interface
        function Add(other: Self): Self;
    end;

    Value = class of IAddToSelf
        function Add(other: Value): Value;
    end;
```

## Generic interfaces

Interfaces can be declared with type params.

```pascal
type IPerson[Job] = interface
    function DoWork(self: Self; job: Job);
end;
```

Interface methods cannot have type params.
It must be possible to fully instantiate an
interface type at compile-time, which means
that the signature of all of its methods must
be known in advance.

```pascal
type IPerson = interface
    // Not valid:
    function DoWork[TJob](self: Self; job: TJob);
end;
```
